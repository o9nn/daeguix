;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Dan9 Contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (gnu dan9 daemons)
             (gnu dan9 persistence)
             (ice-9 threads)
             (srfi srfi-1)
             (srfi srfi-64))

;;;
;;; Tests for Dan9 Persistence Module
;;;

(test-begin "dan9-persistence")

;;; Test checkpoint policy creation

(test-group "checkpoint-policy"
  (test-assert "make-checkpoint-policy creates policy"
    (let ((policy (make-checkpoint-policy 300 #t #f)))
      (checkpoint-policy? policy)))
  
  (test-equal "checkpoint-policy-interval returns interval"
    300
    (let ((policy (make-checkpoint-policy 300 #t #f)))
      (checkpoint-policy-interval policy)))
  
  (test-equal "checkpoint-policy-on-stop returns on-stop"
    #t
    (let ((policy (make-checkpoint-policy 300 #t #f)))
      (checkpoint-policy-on-stop policy))))

;;; Test state serialization and deserialization

(test-group "state-serialization"
  (test-assert "daemon-save-state creates file"
    (let* ((test-daemon (make-daemon "test-save" 'test
                                     #:auto-register #f))
           (test-dir "/tmp/dan9-test-state"))
      (daemon-set-metric! test-daemon 'test-metric 42)
      (let ((result (daemon-save-state test-daemon #:directory test-dir)))
        (and result (string? result)))))
  
  (test-assert "daemon-load-state retrieves state"
    (let* ((test-daemon (make-daemon "test-load" 'test
                                     #:auto-register #f))
           (test-dir "/tmp/dan9-test-state"))
      ;; First save
      (daemon-set-metric! test-daemon 'test-metric 99)
      (daemon-save-state test-daemon #:directory test-dir)
      ;; Then load
      (let ((state (daemon-load-state test-daemon #:directory test-dir)))
        (and state (list? state)))))
  
  (test-assert "saved state contains daemon info"
    (let* ((test-daemon (make-daemon "test-info" 'test
                                     #:auto-register #f))
           (test-dir "/tmp/dan9-test-state"))
      (daemon-save-state test-daemon #:directory test-dir)
      (let ((state (daemon-load-state test-daemon #:directory test-dir)))
        (and state
             (assoc-ref state 'name)
             (equal? (assoc-ref state 'name) "test-info")))))
  
  (test-assert "checkpoint creates state file"
    (let* ((test-daemon (make-daemon "test-checkpoint" 'test
                                     #:auto-register #f))
           (test-dir "/tmp/dan9-test-state"))
      (daemon-checkpoint test-daemon #:directory test-dir))))

;;; Test persistence daemon

(define (simple-loop daemon)
  "Simple test loop."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (usleep 100000)
      (loop))))

(test-group "persistence-daemon"
  (test-assert "make-persistence-daemon creates daemon"
    (let ((pers (make-persistence-daemon
                 #:name "test-persistence"
                 #:state-directory "/tmp/dan9-test-state")))
      (daemon? pers)))
  
  (test-equal "persistence daemon has correct type"
    'persistence
    (let ((pers (make-persistence-daemon #:name "test-pers-type")))
      (daemon-type pers)))
  
  (test-assert "persistence daemon can start"
    (let ((pers (make-persistence-daemon
                 #:name "test-pers-start"
                 #:checkpoint-interval 10)))
      (daemon-start pers persistence-daemon-loop)
      (sleep 1)
      (let ((status (daemon-status pers)))
        (daemon-stop pers)
        (eq? 'running (assoc-ref status 'state)))))
  
  (test-assert "persistence daemon responds to save message"
    (let* ((pers (make-persistence-daemon
                  #:name "test-pers-save-msg"
                  #:state-directory "/tmp/dan9-test-state"))
           (target (make-daemon "test-target" 'test)))
      (daemon-start pers persistence-daemon-loop)
      (daemon-start target simple-loop)
      (sleep 1)
      (daemon-send-message pers pers 'save
                          `((daemon-name . "test-target")))
      (sleep 1)
      (let ((result (daemon-load-state target
                                      #:directory "/tmp/dan9-test-state")))
        (daemon-stop pers)
        (daemon-stop target)
        (and result (list? result))))))

;;; Test checkpoint and restore

(test-group "checkpoint-restore"
  (test-assert "daemon-restore loads checkpoint"
    (let* ((test-daemon (make-daemon "test-restore" 'test
                                     #:auto-register #f))
           (test-dir "/tmp/dan9-test-state"))
      (daemon-set-metric! test-daemon 'counter 123)
      (daemon-checkpoint test-daemon #:directory test-dir)
      (let ((restored (daemon-restore test-daemon #:directory test-dir)))
        (and restored (list? restored))))))

(test-end "dan9-persistence")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
