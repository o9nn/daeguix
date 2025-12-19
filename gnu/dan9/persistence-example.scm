#!/usr/bin/env -S guile -L .
!#

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
             (srfi srfi-1))

;;;
;;; Dan9 Persistence Example
;;;
;;; This demonstrates daemon state persistence and recovery.
;;;

(define (worker-daemon-loop daemon)
  "Simple worker daemon that counts tasks."
  (let loop ((count 0))
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (case (message-type msg)
            ((task)
             (set! count (+ count 1))
             (daemon-set-metric! daemon 'tasks-processed count)
             (format #t "[~a] Processed task #~a~%"
                     (daemon-name daemon) count))
            ((status)
             (format #t "[~a] Status: ~a tasks processed~%"
                     (daemon-name daemon) count)))))
      
      (usleep 100000) ; 100ms
      (loop count))))

(define (demo-persistence)
  "Demonstrate persistence functionality."
  (format #t "~%=== Dan9 Persistence Example ===~%~%")
  
  ;; Create persistence daemon
  (format #t "1. Creating persistence daemon...~%")
  (define pers (make-persistence-daemon
                #:state-directory "/tmp/dan9-state-demo"
                #:checkpoint-interval 10))
  (daemon-start pers persistence-daemon-loop)
  (sleep 1)
  
  ;; Create worker daemons
  (format #t "~%2. Creating worker daemons...~%")
  (define worker1 (make-daemon "worker-1" 'worker))
  (define worker2 (make-daemon "worker-2" 'worker))
  (daemon-start worker1 worker-daemon-loop)
  (daemon-start worker2 worker-daemon-loop)
  (sleep 1)
  
  ;; Send some work to workers
  (format #t "~%3. Sending tasks to workers...~%")
  (daemon-send-message pers worker1 'task '((data . "task-1")))
  (daemon-send-message pers worker1 'task '((data . "task-2")))
  (daemon-send-message pers worker1 'task '((data . "task-3")))
  (daemon-send-message pers worker2 'task '((data . "task-a")))
  (daemon-send-message pers worker2 'task '((data . "task-b")))
  (sleep 1)
  
  ;; Check status
  (format #t "~%4. Checking worker status...~%")
  (daemon-send-message pers worker1 'status '())
  (daemon-send-message pers worker2 'status '())
  (sleep 1)
  
  ;; Manual checkpoint
  (format #t "~%5. Creating manual checkpoint...~%")
  (daemon-send-message pers pers 'checkpoint-all '())
  (sleep 1)
  
  ;; Save individual daemon states
  (format #t "~%6. Saving individual daemon states...~%")
  (daemon-save-state worker1 #:directory "/tmp/dan9-state-demo")
  (daemon-save-state worker2 #:directory "/tmp/dan9-state-demo")
  (sleep 1)
  
  ;; List checkpoints
  (format #t "~%7. Listing available checkpoints...~%")
  (daemon-send-message pers pers 'list '())
  (sleep 1)
  
  ;; Demonstrate loading state
  (format #t "~%8. Loading saved state...~%")
  (let ((state1 (daemon-load-state worker1 #:directory "/tmp/dan9-state-demo"))
        (state2 (daemon-load-state worker2 #:directory "/tmp/dan9-state-demo")))
    (format #t "Worker-1 saved state: ~a~%" state1)
    (format #t "Worker-2 saved state: ~a~%" state2))
  (sleep 1)
  
  ;; Stop daemons
  (format #t "~%9. Stopping daemons...~%")
  (daemon-stop worker1)
  (daemon-stop worker2)
  (daemon-stop pers)
  (sleep 1)
  
  (format #t "~%=== Persistence Example Complete ===~%~%")
  (format #t "State files saved to: /tmp/dan9-state-demo/~%")
  (format #t "Try running again to demonstrate recovery!~%~%"))

;; Run the demo
(demo-persistence)
