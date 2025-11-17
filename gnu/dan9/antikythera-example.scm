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
;;; Antikythera Example: Years Scaled to Days
;;;
;;; This example demonstrates the Antikythera mechanism with nested event
;;; loops, scaling astronomical time periods (years) down to manageable
;;; simulation time (days).
;;;

(add-to-load-path (dirname (current-filename)))

(use-modules (gnu dan9 daemons)
             (gnu dan9 antikythera)
             (gnu dan9 egregore))

(define (main)
  (display "\n=== Antikythera Mechanism: Nested Event Loop Scheduler ===\n\n")
  
  (display "The Antikythera mechanism scales time like gears:\n")
  (display "  - 1 year (365 days) -> 1 day (simulated)\n")
  (display "  - 1 day (24 hours) -> 1 hour (simulated)\n")
  (display "  - 1 hour (60 minutes) -> 1 minute (simulated)\n\n")
  
  ;; Create the Antikythera scheduler daemon
  (display "Creating Antikythera scheduler...\n")
  (define antikythera (make-antikythera-daemon
                       #:name "antikythera"
                       #:base-tick-ms 100)) ; Base tick every 100ms
  
  (display "  ✓ Antikythera scheduler created\n\n")
  
  ;; Create nested gears (year -> day -> hour -> minute)
  (display "Creating nested gear hierarchy:\n")
  
  ;; Innermost gear: minutes (tick every 100ms = simulated minute)
  (define minute-gear (make-gear "minute" 1))
  (display "  ✓ Minute gear (ratio: 1) - base level\n")
  
  ;; Hour gear: 60 minutes = 1 hour
  (define hour-gear (make-gear "hour" 60 #:parent-gear minute-gear))
  (display "  ✓ Hour gear (ratio: 60) - 60 ticks = 1 simulated hour\n")
  
  ;; Day gear: 24 hours = 1 day
  (define day-gear (make-gear "day" 24 #:parent-gear hour-gear))
  (display "  ✓ Day gear (ratio: 24) - 24 ticks = 1 simulated day\n")
  
  ;; Year gear: 365 days = 1 year
  (define year-gear (make-gear "year" 365 #:parent-gear day-gear))
  (display "  ✓ Year gear (ratio: 365) - 365 ticks = 1 simulated year\n\n")
  
  ;; Register events on different gears
  (display "Registering events:\n")
  
  ;; Every 10 minutes (in simulated time)
  (register-event! minute-gear "minute-event" 10
                   (lambda (gear tick)
                     (format #t "  [~a ticks] Minute event fired (every 10 simulated minutes)~%"
                             tick)))
  
  ;; Every 2 hours (in simulated time)
  (register-event! hour-gear "hour-event" 2
                   (lambda (gear tick)
                     (format #t "  [~a ticks] Hour event fired (every 2 simulated hours)~%"
                             tick)))
  
  ;; Every 7 days (weekly in simulated time)
  (register-event! day-gear "week-event" 7
                   (lambda (gear tick)
                     (format #t "  [~a ticks] Weekly event fired (every 7 simulated days)~%"
                             tick)))
  
  ;; Every year (in simulated time)
  (register-event! year-gear "year-event" 1
                   (lambda (gear tick)
                     (format #t "  [~a ticks] YEAR EVENT FIRED! (1 simulated year completed)~%"
                             tick)))
  
  (display "  ✓ Minute event: fires every 10 ticks (10 simulated minutes)\n")
  (display "  ✓ Hour event: fires every 2 ticks (2 simulated hours)\n")
  (display "  ✓ Week event: fires every 7 ticks (7 simulated days)\n")
  (display "  ✓ Year event: fires every 1 tick (1 simulated year)\n\n")
  
  ;; Register gears with the Antikythera daemon
  (display "Registering gears with Antikythera...\n")
  (daemon-start antikythera antikythera-daemon-loop)
  (sleep 0.2) ; Let daemon start
  
  (daemon-send-message antikythera antikythera 'register-gear
                      `((gear . ,minute-gear)))
  (daemon-send-message antikythera antikythera 'register-gear
                      `((gear . ,hour-gear)))
  (daemon-send-message antikythera antikythera 'register-gear
                      `((gear . ,day-gear)))
  (daemon-send-message antikythera antikythera 'register-gear
                      `((gear . ,year-gear)))
  
  (display "  ✓ All gears registered\n\n")
  
  ;; Run the simulation
  (display "Starting time simulation (running for 5 seconds)...\n")
  (display "Watch as nested event loops fire at different time scales:\n\n")
  
  (sleep 5)
  
  ;; Get status
  (newline)
  (display "=== Simulation Results ===\n\n")
  
  (display "Gear tick counts (after 5 seconds):\n")
  (format #t "  Minute gear: ~a ticks (simulated minutes)~%"
          (gear-tick-count minute-gear))
  (format #t "  Hour gear: ~a ticks (simulated hours = ~a simulated minutes)~%"
          (gear-tick-count hour-gear)
          (* (gear-tick-count hour-gear) 60))
  (format #t "  Day gear: ~a ticks (simulated days = ~a simulated hours)~%"
          (gear-tick-count day-gear)
          (* (gear-tick-count day-gear) 24))
  (format #t "  Year gear: ~a ticks (simulated years = ~a simulated days)~%"
          (gear-tick-count year-gear)
          (* (gear-tick-count year-gear) 365))
  
  (newline)
  (display "Time scaling demonstration:\n")
  (format #t "  In 5 real seconds, we simulated:\n")
  (format #t "    - ~a minutes\n" (gear-tick-count minute-gear))
  (format #t "    - ~a hours\n" (gear-tick-count hour-gear))
  (format #t "    - ~a days\n" (gear-tick-count day-gear))
  (format #t "    - ~a years\n" (gear-tick-count year-gear))
  
  (when (> (gear-tick-count year-gear) 0)
    (format #t "  ✓ Successfully completed ~a simulated year(s)!~%"
            (gear-tick-count year-gear)))
  
  (newline)
  (display "Antikythera daemon status:\n")
  (let ((status (antikythera-status antikythera)))
    (format #t "  State: ~a~%" (assoc-ref status 'state))
    (format #t "  Total ticks: ~a~%" (assoc-ref status 'total-ticks))
    (format #t "  Registered gears: ~a~%"
            (length (assoc-ref status 'gears))))
  
  ;; Stop the daemon
  (newline)
  (display "Stopping Antikythera scheduler...\n")
  (daemon-stop antikythera)
  (display "  ✓ Scheduler stopped\n\n")
  
  (display "=== Antikythera Example Complete ===\n\n")
  (display "The Antikythera mechanism demonstrates:\n")
  (display "  1. Nested event loops with different time scales\n")
  (display "  2. Gear-based scheduling metaphor\n")
  (display "  3. Time scaling (years -> days, days -> hours, etc.)\n")
  (display "  4. Hierarchical tick propagation\n")
  (display "  5. Event registration and dispatch\n\n")
  
  (display "Applications:\n")
  (display "  - Simulating long-term processes in compressed time\n")
  (display "  - Astronomical cycle modeling (like the original Antikythera)\n")
  (display "  - Multi-timescale system coordination\n")
  (display "  - Hierarchical task scheduling\n\n"))

;; Run the example
(main)
