;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 OpenCog Orchestration Contributors
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

(define-module (test-opencog-daemons)
  #:use-module (gnu opencog daemons)
  #:use-module (srfi srfi-64))

(test-begin "opencog-daemons")

;; Resource Monitor Daemon Tests
(test-assert "resource-monitor-daemon creation"
  (let ((daemon (make-resource-monitor-daemon #:interval 10)))
    (daemon? daemon)))

(test-assert "daemon start and stop"
  (let ((daemon (make-resource-monitor-daemon #:interval 1)))
    (daemon-start daemon)
    (sleep 2)
    (daemon-stop daemon)
    #t))

(test-assert "daemon status"
  (let ((daemon (make-resource-monitor-daemon)))
    (let ((status (daemon-status daemon)))
      (and (list? status)
           (assoc-ref status 'name)
           (assoc-ref status 'type)
           (assoc-ref status 'state)))))

(test-assert "daemon metrics collection"
  (let ((daemon (make-resource-monitor-daemon #:interval 1)))
    (daemon-start daemon)
    (sleep 2)
    (let ((metrics (daemon-get-metrics daemon)))
      (daemon-stop daemon)
      (list? metrics))))

;; AtomSpace Sync Daemon Tests
(test-assert "atomspace-sync-daemon creation"
  (let ((daemon (make-atomspace-sync-daemon #:sync-interval 30)))
    (daemon? daemon)))

;; Attention Allocation Daemon Tests
(test-assert "attention-allocation-daemon creation"
  (let ((daemon (make-attention-allocation-daemon #:allocation-interval 5)))
    (daemon? daemon)))

;; Health Monitor Daemon Tests
(test-assert "health-monitor-daemon creation"
  (let ((daemon (make-health-monitor-daemon #:check-interval 15)))
    (daemon? daemon)))

(test-assert "multiple daemons running"
  (let ((d1 (make-resource-monitor-daemon #:interval 1))
        (d2 (make-health-monitor-daemon #:check-interval 1)))
    (daemon-start d1)
    (daemon-start d2)
    (sleep 2)
    (daemon-stop d1)
    (daemon-stop d2)
    #t))

(test-end "opencog-daemons")
