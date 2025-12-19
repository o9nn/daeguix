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
             (gnu dan9 monitoring)
             (gnu dan9 logging)
             (gnu dan9 timer)
             (ice-9 threads)
             (srfi srfi-1))

;;;
;;; Dan9 Integrated System Example
;;;
;;; Demonstrates all new daemons working together:
;;; - Persistence for state management
;;; - Monitoring for system observation
;;; - Logging for centralized logs
;;; - Timer for scheduled tasks
;;;

(define (worker-daemon-loop daemon)
  "Worker daemon that logs and reports metrics."
  (let loop ((count 0))
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (case (message-type msg)
            ((work)
             (set! count (+ count 1))
             (daemon-set-metric! daemon 'tasks-completed count)
             (log-info (daemon-name daemon)
                      (format #f "Completed task #~a" count)))
            ((status)
             (log-info (daemon-name daemon)
                      (format #f "Status: ~a tasks completed" count))))))
      
      (usleep 100000) ; 100ms
      (loop count))))

(define (demo-integrated-system)
  "Demonstrate integrated Dan9 system."
  (format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║        DAN9 INTEGRATED SYSTEM DEMONSTRATION                   ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
  
  ;; 1. Start logging daemon
  (format #t "1. Starting logging daemon...~%")
  (define logger (make-logging-daemon #:min-level 'info))
  (daemon-start logger logging-daemon-loop)
  (sleep 1)
  
  (log-info "system" "Dan9 integrated system starting up")
  
  ;; 2. Start monitoring daemon
  (format #t "~%2. Starting monitoring daemon...~%")
  (define monitor (make-monitoring-daemon #:collect-interval 3))
  (daemon-start monitor monitoring-daemon-loop)
  (sleep 1)
  
  (log-info "system" "Monitoring daemon initialized")
  
  ;; 3. Start persistence daemon
  (format #t "~%3. Starting persistence daemon...~%")
  (define pers (make-persistence-daemon
                #:state-directory "/tmp/dan9-integrated"
                #:checkpoint-interval 10))
  (daemon-start pers persistence-daemon-loop)
  (sleep 1)
  
  (log-info "system" "Persistence daemon initialized")
  
  ;; 4. Start timer daemon
  (format #t "~%4. Starting timer daemon...~%")
  (define timer (make-timer-daemon #:tick-interval 1))
  (daemon-start timer timer-daemon-loop)
  (sleep 1)
  
  (log-info "system" "Timer daemon initialized")
  
  ;; 5. Create worker daemons
  (format #t "~%5. Creating worker daemons...~%")
  (define worker1 (make-daemon "worker-1" 'worker))
  (define worker2 (make-daemon "worker-2" 'worker))
  (daemon-start worker1 worker-daemon-loop)
  (daemon-start worker2 worker-daemon-loop)
  (sleep 1)
  
  (log-info "system" "Worker daemons created and started")
  
  ;; 6. Schedule periodic tasks
  (format #t "~%6. Scheduling periodic tasks...~%")
  (schedule-repeating-task 3 "worker-1" 'work '((type . "periodic")))
  (schedule-repeating-task 4 "worker-2" 'work '((type . "periodic")))
  (schedule-repeating-task 5 "monitoring" 'dashboard '())
  (sleep 1)
  
  (log-info "system" "Periodic tasks scheduled")
  
  ;; 7. List scheduled tasks
  (format #t "~%7. Listing scheduled tasks...~%")
  (list-scheduled-tasks)
  (sleep 1)
  
  ;; 8. Send some work
  (format #t "~%8. Sending work to workers...~%")
  (for-each
   (lambda (i)
     (daemon-send-message timer worker1 'work '((manual . #t)))
     (daemon-send-message timer worker2 'work '((manual . #t))))
   (iota 3))
  (sleep 2)
  
  (log-info "system" "Manual work dispatched")
  
  ;; 9. Display dashboard
  (format #t "~%9. Displaying monitoring dashboard...~%")
  (daemon-send-message timer monitor 'dashboard '())
  (sleep 2)
  
  ;; 10. Create checkpoint
  (format #t "~%10. Creating system checkpoint...~%")
  (daemon-send-message timer pers 'checkpoint-all '())
  (sleep 2)
  
  (log-info "system" "System checkpoint created")
  
  ;; 11. Display logs
  (format #t "~%11. Displaying recent logs...~%")
  (daemon-send-message timer logger 'display '((limit . 15)))
  (sleep 2)
  
  ;; 12. Show timer stats
  (format #t "~%12. Timer statistics...~%")
  (daemon-send-message timer timer 'stats '())
  (sleep 1)
  
  ;; 13. Show monitoring metrics
  (format #t "~%13. Showing metrics table...~%")
  (daemon-send-message timer monitor 'metrics-table
                      '((metric-name . tasks-completed)))
  (sleep 2)
  
  ;; 14. Wait for scheduled tasks to fire
  (format #t "~%14. Waiting for scheduled tasks (15 seconds)...~%")
  (sleep 15)
  
  ;; 15. Final dashboard
  (format #t "~%15. Final dashboard view...~%")
  (daemon-send-message timer monitor 'dashboard '())
  (sleep 2)
  
  ;; 16. Final logs
  (format #t "~%16. Final log entries...~%")
  (daemon-send-message timer logger 'display '((limit . 20)))
  (sleep 2)
  
  (log-info "system" "Dan9 integrated system demonstration complete")
  
  ;; 17. Cleanup
  (format #t "~%17. Stopping all daemons...~%")
  (daemon-stop worker1)
  (daemon-stop worker2)
  (daemon-stop timer)
  (daemon-stop pers)
  (daemon-stop monitor)
  (daemon-stop logger)
  (sleep 2)
  
  (format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║           INTEGRATED SYSTEM DEMONSTRATION COMPLETE            ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
  (format #t "System demonstrated:~%")
  (format #t "  ✓ Centralized logging~%")
  (format #t "  ✓ Real-time monitoring~%")
  (format #t "  ✓ State persistence~%")
  (format #t "  ✓ Scheduled task execution~%")
  (format #t "  ✓ Inter-daemon coordination~%~%"))

;; Run the demo
(demo-integrated-system)
