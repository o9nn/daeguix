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
             (gnu dan9 monitoring)
             (ice-9 threads)
             (srfi srfi-1))

;;;
;;; Dan9 Monitoring Dashboard Example
;;;
;;; This demonstrates real-time daemon monitoring and dashboard.
;;;

(define (worker-daemon-loop daemon)
  "Worker daemon that processes tasks and updates metrics."
  (let loop ((tasks 0)
             (errors 0))
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (case (message-type msg)
            ((task)
             (set! tasks (+ tasks 1))
             (daemon-set-metric! daemon 'tasks-completed tasks)
             (daemon-set-metric! daemon 'last-task-time
                               (time-second (current-time time-monotonic))))
            ((error)
             (set! errors (+ errors 1))
             (daemon-set-metric! daemon 'errors errors)))))
      
      ;; Update load metric
      (daemon-set-metric! daemon 'cpu-load (+ 0.1 (random:uniform)))
      (daemon-set-metric! daemon 'memory-mb (+ 50 (random 100)))
      
      (usleep 200000) ; 200ms
      (loop tasks errors))))

(define (demo-monitoring)
  "Demonstrate monitoring dashboard functionality."
  (format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║         DAN9 MONITORING DASHBOARD DEMONSTRATION               ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
  
  ;; Create monitoring daemon
  (format #t "1. Starting monitoring daemon...~%")
  (define monitor (make-monitoring-daemon
                   #:collect-interval 2))
  (daemon-start monitor monitoring-daemon-loop)
  (sleep 1)
  
  ;; Create worker daemons
  (format #t "~%2. Creating worker daemons...~%")
  (define worker1 (make-daemon "worker-alpha" 'worker))
  (define worker2 (make-daemon "worker-beta" 'worker))
  (define worker3 (make-daemon "worker-gamma" 'worker))
  (daemon-start worker1 worker-daemon-loop)
  (daemon-start worker2 worker-daemon-loop)
  (daemon-start worker3 worker-daemon-loop)
  (sleep 1)
  
  ;; Send some work to workers
  (format #t "~%3. Sending tasks to workers...~%")
  (for-each
   (lambda (worker)
     (daemon-send-message monitor worker 'task '((data . "process")))
     (daemon-send-message monitor worker 'task '((data . "compute")))
     (daemon-send-message monitor worker 'task '((data . "analyze"))))
   (list worker1 worker2 worker3))
  (sleep 2)
  
  ;; Display initial dashboard
  (format #t "~%4. Displaying initial dashboard...~%")
  (daemon-send-message monitor monitor 'dashboard '())
  (sleep 1)
  
  ;; Collect metrics over time
  (format #t "~%5. Collecting metrics (waiting 5 seconds)...~%")
  (sleep 5)
  
  ;; Take a snapshot
  (format #t "~%6. Taking system snapshot...~%")
  (daemon-send-message monitor monitor 'snapshot '())
  (sleep 1)
  
  ;; Display individual daemon status
  (format #t "~%7. Displaying individual daemon status...~%")
  (daemon-send-message monitor monitor 'status
                      '((daemon-name . "worker-alpha")))
  (sleep 1)
  
  ;; Send more work to create history
  (format #t "~%8. Generating more activity...~%")
  (for-each
   (lambda (i)
     (let ((worker (case (modulo i 3)
                     ((0) worker1)
                     ((1) worker2)
                     ((2) worker3))))
       (daemon-send-message monitor worker 'task '((data . "work")))
       (usleep 100000)))
   (iota 30))
  (sleep 3)
  
  ;; Display metrics table
  (format #t "~%9. Displaying metrics table...~%")
  (daemon-send-message monitor monitor 'metrics-table
                      '((metric-name . tasks-completed)))
  (sleep 1)
  
  ;; Show aggregate statistics
  (format #t "~%10. Computing aggregate statistics...~%")
  (daemon-send-message monitor monitor 'aggregate
                      '((daemon-name . "worker-alpha")
                        (metric-name . tasks-completed)))
  (sleep 1)
  
  ;; Display final dashboard
  (format #t "~%11. Displaying final dashboard...~%")
  (daemon-send-message monitor monitor 'dashboard '())
  (sleep 1)
  
  ;; Cleanup
  (format #t "~%12. Stopping all daemons...~%")
  (daemon-stop worker1)
  (daemon-stop worker2)
  (daemon-stop worker3)
  (daemon-stop monitor)
  (sleep 1)
  
  (format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║           MONITORING DEMONSTRATION COMPLETE                   ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%"))

;; Run the demo
(demo-monitoring)
