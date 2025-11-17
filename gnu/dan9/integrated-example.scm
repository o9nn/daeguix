#!/usr/bin/env -S guile --no-auto-compile
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

;;;
;;; Combined Example: Egregores with Antikythera Scheduling
;;;
;;; This example demonstrates using egregore orchestration patterns with
;;; the Antikythera time-scaled scheduler to create a complex multi-daemon
;;; system that operates across different time scales.
;;;

(add-to-load-path (dirname (current-filename)))

(use-modules (gnu dan9 daemons)
             (gnu dan9 egregore)
             (gnu dan9 antikythera))

(define (main)
  (display "\n=== Egregore + Antikythera: Complex Daemon Orchestration ===\n\n")
  
  (display "Scenario: Simulating a distributed system with multiple time scales\n")
  (display "  - Fast workers: process tasks every few seconds (simulated minutes)\n")
  (display "  - Daily coordinator: aggregates results daily (simulated)\n")
  (display "  - Weekly reporter: generates reports weekly (simulated)\n")
  (display "  - Yearly archiver: archives data yearly (simulated)\n\n")
  
  ;; Create the Antikythera scheduler
  (display "Creating Antikythera time-scaled scheduler...\n")
  (define scheduler (make-antikythera-daemon #:base-tick-ms 100))
  
  ;; Create time gears
  (define minute-gear (make-gear "minute" 1))
  (define hour-gear (make-gear "hour" 60 #:parent-gear minute-gear))
  (define day-gear (make-gear "day" 24 #:parent-gear hour-gear))
  (define week-gear (make-gear "week" 7 #:parent-gear day-gear))
  (define year-gear (make-gear "year" 52 #:parent-gear week-gear))
  
  (display "  ✓ Created 5-level time hierarchy: minute -> hour -> day -> week -> year\n\n")
  
  ;; Create worker daemons
  (display "Creating worker daemons...\n")
  (define worker1 (make-daemon "fast-worker-1" 'worker #:auto-register #f))
  (define worker2 (make-daemon "fast-worker-2" 'worker #:auto-register #f))
  (define worker3 (make-daemon "fast-worker-3" 'worker #:auto-register #f))
  (define daily-coordinator (make-daemon "daily-coord" 'coordinator #:auto-register #f))
  (define weekly-reporter (make-daemon "weekly-reporter" 'reporter #:auto-register #f))
  (define yearly-archiver (make-daemon "yearly-archiver" 'archiver #:auto-register #f))
  (display "  ✓ Created 6 daemons at different organizational levels\n\n")
  
  ;; Create egregore for fast workers (swarm)
  (display "Creating worker swarm egregore...\n")
  (define worker-swarm (make-swarm-egregore "fast-workers"
                                           #:daemons (list worker1 worker2 worker3)))
  (egregore-start worker-swarm)
  (display "  ✓ Worker swarm started (3 fast workers)\n\n")
  
  ;; Create egregore for reporting hierarchy
  (display "Creating reporting hierarchy egregore...\n")
  (define reporting-hierarchy (make-hierarchy-egregore "reporting"
                                                      #:daemons (list daily-coordinator
                                                                    weekly-reporter
                                                                    yearly-archiver)))
  (egregore-start reporting-hierarchy)
  (display "  ✓ Reporting hierarchy started (3 levels)\n\n")
  
  ;; Start the Antikythera scheduler
  (display "Starting Antikythera scheduler...\n")
  (antikythera-start scheduler)
  (sleep 0.2)
  
  ;; Register gears with scheduler
  (daemon-send-message scheduler scheduler 'register-gear `((gear . ,minute-gear)))
  (daemon-send-message scheduler scheduler 'register-gear `((gear . ,hour-gear)))
  (daemon-send-message scheduler scheduler 'register-gear `((gear . ,day-gear)))
  (daemon-send-message scheduler scheduler 'register-gear `((gear . ,week-gear)))
  (daemon-send-message scheduler scheduler 'register-gear `((gear . ,year-gear)))
  (display "  ✓ All time gears registered\n\n")
  
  ;; Register events that trigger egregore actions
  (display "Registering time-scaled events...\n")
  
  ;; Every 5 minutes: trigger worker swarm
  (register-event! minute-gear "worker-task" 5
                  (lambda (gear tick)
                    (format #t "  [~a min] Workers processing tasks...~%" tick)
                    (egregore-broadcast worker-swarm 'process-task
                                      `((task-id . ,(+ 1000 tick))))))
  
  ;; Every 2 hours: status update
  (register-event! hour-gear "status-update" 2
                  (lambda (gear tick)
                    (format #t "  [~a hr] Status update from coordinators~%" tick)))
  
  ;; Every day: daily coordination
  (register-event! day-gear "daily-coordination" 1
                  (lambda (gear tick)
                    (format #t "  [Day ~a] Daily coordinator aggregating results~%" tick)
                    (daemon-send-message daily-coordinator daily-coordinator
                                       'aggregate '((type . daily)))))
  
  ;; Every week: weekly report
  (register-event! week-gear "weekly-report" 1
                  (lambda (gear tick)
                    (format #t "  [Week ~a] Weekly reporter generating report~%" tick)
                    (daemon-send-message weekly-reporter weekly-reporter
                                       'generate-report '((type . weekly)))))
  
  ;; Every year: archive data
  (register-event! year-gear "yearly-archive" 1
                  (lambda (gear tick)
                    (format #t "\n*** [YEAR ~a] YEARLY ARCHIVER: Archiving all data! ***\n\n" tick)
                    (daemon-send-message yearly-archiver yearly-archiver
                                       'archive '((year . ,tick)))))
  
  (display "  ✓ Events registered across all time scales\n\n")
  
  ;; Run the simulation
  (display "Running integrated simulation (5 seconds)...\n")
  (display "Watch egregores react to time-scaled events:\n\n")
  
  (sleep 5)
  
  ;; Results
  (newline)
  (display "=== Simulation Results ===\n\n")
  
  (display "Time progression:\n")
  (format #t "  Simulated time passed:\n")
  (format #t "    ~a minutes\n" (gear-tick-count minute-gear))
  (format #t "    ~a hours\n" (gear-tick-count hour-gear))
  (format #t "    ~a days\n" (gear-tick-count day-gear))
  (format #t "    ~a weeks\n" (gear-tick-count week-gear))
  (format #t "    ~a years\n" (gear-tick-count year-gear))
  
  (newline)
  (display "Egregore states:\n")
  (let ((swarm-status (egregore-status worker-swarm))
        (hierarchy-status (egregore-status reporting-hierarchy)))
    (format #t "  Worker Swarm: ~a daemons, state: ~a\n"
            (assoc-ref swarm-status 'daemon-count)
            (assoc-ref swarm-status 'coordinator-state))
    (format #t "  Reporting Hierarchy: ~a daemons, state: ~a\n"
            (assoc-ref hierarchy-status 'daemon-count)
            (assoc-ref hierarchy-status 'coordinator-state)))
  
  (newline)
  (display "Scheduler metrics:\n")
  (let ((status (antikythera-status scheduler)))
    (format #t "  Total scheduler ticks: ~a\n"
            (assoc-ref status 'total-ticks))
    (format #t "  Gears managed: ~a\n"
            (length (assoc-ref status 'gears))))
  
  ;; Clean up
  (newline)
  (display "Shutting down system...\n")
  (egregore-stop worker-swarm)
  (egregore-stop reporting-hierarchy)
  (antikythera-stop scheduler)
  (display "  ✓ All egregores and scheduler stopped\n\n")
  
  (display "=== Integration Example Complete ===\n\n")
  
  (display "Key Concepts Demonstrated:\n\n")
  
  (display "1. Multi-Scale Time Management:\n")
  (display "   - Antikythera provides 5 nested time scales\n")
  (display "   - Events fire at different rates (minutes to years)\n")
  (display "   - Real-time compression: simulate years in seconds\n\n")
  
  (display "2. Egregore Orchestration:\n")
  (display "   - Swarm pattern for parallel worker coordination\n")
  (display "   - Hierarchy pattern for reporting structure\n")
  (display "   - Automatic message routing through coordinators\n\n")
  
  (display "3. Integration:\n")
  (display "   - Time-scaled events trigger egregore actions\n")
  (display "   - Multiple orchestration patterns coexist\n")
  (display "   - Complex multi-daemon systems with simple primitives\n\n")
  
  (display "Applications:\n")
  (display "  - Distributed system simulation\n")
  (display "  - Long-term process modeling\n")
  (display "  - Multi-timescale workflow orchestration\n")
  (display "  - Hierarchical task scheduling with time constraints\n\n"))

;; Run the example
(main)
