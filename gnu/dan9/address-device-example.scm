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
;;; D9 Address System & Virtual Devices Example
;;;
;;; Demonstrates:
;;; 1. Address-based routing instead of namespaces
;;; 2. Thread pool management with instant cycle spawn/drop
;;; 3. Virtual devices initiated by egregores
;;; 4. Gesture device with spatio-temporal vectors
;;; 5. Clockwork device with nested gearing arrays
;;;

(add-to-load-path (dirname (current-filename)))

(use-modules (gnu dan9 daemons)
             (gnu dan9 egregore)
             (gnu dan9 antikythera)
             (gnu dan9 address)
             (gnu dan9 device))

(define (main)
  (display "\n=== D9 Address System & Virtual Devices ===\n\n")
  
  ;; Part 1: Address System
  (display "=== Part 1: Address-Based Routing ===\n\n")
  
  (display "Creating address router...\n")
  (define router (make-address-router))
  (display "  ✓ Address router created\n\n")
  
  (display "Creating daemons and registering with addresses...\n")
  (define worker1 (make-daemon "worker-1" 'worker #:auto-register #f))
  (define worker2 (make-daemon "worker-2" 'worker #:auto-register #f))
  (define coordinator (make-daemon "coordinator" 'coordinator #:auto-register #f))
  
  ;; Register with D9 addresses
  (router-register! router "d9://egregore/workers/worker-1" worker1)
  (router-register! router "d9://egregore/workers/worker-2" worker2)
  (router-register! router "d9://egregore/coordinator/main" coordinator)
  (display "  ✓ 3 daemons registered with D9 addresses\n\n")
  
  (display "Resolving addresses...\n")
  (let ((addr1 (router-resolve router "d9://egregore/workers/worker-1"))
        (addr2 (router-resolve router "d9://egregore/coordinator/main")))
    (format #t "  Resolved: ~a -> daemon: ~a~%"
            (address-uri addr1)
            (daemon-name (address-daemon addr1)))
    (format #t "  Resolved: ~a -> daemon: ~a~%"
            (address-uri addr2)
            (daemon-name (address-daemon addr2))))
  (newline)
  
  ;; Part 2: Thread Pool Management
  (display "=== Part 2: Thread Pool - Instant Cycle Spawn/Drop ===\n\n")
  
  (display "Creating thread pool...\n")
  (define pool (make-thread-pool "event-loops" #:size 10))
  (display "  ✓ Thread pool created (size: 10)\n\n")
  
  (display "Spawning 5 execution cycles...\n")
  (let spawn-loop ((i 0))
    (when (< i 5)
      (thread-pool-spawn-cycle! pool (lambda () (usleep 100000)))
      (spawn-loop (+ i 1))))
  (display "  ✓ 5 cycles spawned\n\n")
  
  (display "Synchronizing to 100 cycles (instantaneous scaling)...\n")
  (thread-pool-sync-cycles! pool 100)
  (sleep 0.2)
  (display "  ✓ Scaled to 100 cycles\n\n")
  
  (display "Dropping back to 10 cycles (instantaneous)...\n")
  (thread-pool-sync-cycles! pool 10)
  (sleep 0.2)
  (display "  ✓ Scaled down to 10 cycles\n\n")
  
  (display "Demo: Scaling from 1 to 1000 cycles instantaneously...\n")
  (thread-pool-sync-cycles! pool 1000)
  (sleep 0.1)
  (format #t "  ✓ ~a cycles active\n" (length (thread-pool-cycles pool)))
  (thread-pool-sync-cycles! pool 1)
  (sleep 0.1)
  (format #t "  ✓ Scaled back to ~a cycle\n\n" (length (thread-pool-cycles pool)))
  
  ;; Part 3: Virtual Devices - Gesture
  (display "=== Part 3: Gesture Device (Spatio-Temporal Vectors) ===\n\n")
  
  (display "Creating gesture device (like touchpad with intent)...\n")
  (define touchpad (make-gesture-device "touchpad" #:sensitivity 1.5))
  (display "  ✓ Gesture device created\n\n")
  
  (display "Starting gesture device daemon...\n")
  (device-start touchpad gesture-daemon-loop)
  (sleep 0.1)
  (display "  ✓ Device daemon running\n\n")
  
  (display "Simulating gesture inputs...\n")
  ;; Simulate a swipe gesture
  (device-send-event touchpad 'gesture-input
                    '((x . 100) (y . 100) (z . 1) (velocity . 0)))
  (usleep 50000)
  (device-send-event touchpad 'gesture-input
                    '((x . 120) (y . 105) (z . 1) (velocity . 5)))
  (usleep 50000)
  (device-send-event touchpad 'gesture-input
                    '((x . 150) (y . 110) (z . 1) (velocity . 15)))
  (usleep 50000)
  (device-send-event touchpad 'gesture-input
                    '((x . 190) (y . 115) (z . 1) (velocity . 20)))
  (sleep 0.2)
  (display "  ✓ Gesture sequence recorded\n\n")
  
  (display "Recognizing gesture intent...\n")
  (device-send-event touchpad 'recognize '())
  (sleep 0.2)
  
  (device-stop touchpad)
  (display "  ✓ Gesture device stopped\n\n")
  
  ;; Part 4: Virtual Devices - Clockwork
  (display "=== Part 4: Clockwork Device (Nested Gearing Arrays) ===\n\n")
  
  (display "Creating clockwork device with nested gears...\n")
  (define clockwork (make-clockwork-device "mechanism"))
  (display "  ✓ Clockwork device created\n\n")
  
  (display "Adding nested gears (like Antikythera mechanism)...\n")
  (define gear1 (make-gear "seconds" 60))
  (define gear2 (make-gear "minutes" 60 #:parent-gear gear1))
  (define gear3 (make-gear "hours" 24 #:parent-gear gear2))
  
  (clockwork-add-gear! clockwork gear1)
  (clockwork-add-gear! clockwork gear2)
  (clockwork-add-gear! clockwork gear3)
  (display "  ✓ 3 gears added to clockwork array\n\n")
  
  (display "Ticking clockwork device...\n")
  (let tick-loop ((i 0))
    (when (< i 10)
      (clockwork-tick! clockwork)
      (tick-loop (+ i 1))))
  (display "  ✓ Clockwork ticked 10 times\n\n")
  
  ;; Part 5: Egregore-Device Integration
  (display "=== Part 5: Egregore Initiates Virtual Devices ===\n\n")
  
  (display "Creating swarm egregore...\n")
  (define device-swarm (make-swarm-egregore "device-controllers"))
  (display "  ✓ Swarm egregore created\n\n")
  
  (display "Creating virtual devices for the swarm...\n")
  (define sensor1 (make-virtual-device "temp-sensor" 'sensor))
  (define sensor2 (make-virtual-device "pressure-sensor" 'sensor))
  (display "  ✓ 2 sensor devices created\n\n")
  
  (display "Egregore initiating devices...\n")
  (egregore-add-daemon! device-swarm (device-daemon sensor1))
  (egregore-add-daemon! device-swarm (device-daemon sensor2))
  (display "  ✓ Devices added to egregore\n\n")
  
  (display "Starting egregore...\n")
  (egregore-start device-swarm)
  (sleep 0.2)
  (display "  ✓ Egregore coordinating virtual devices\n\n")
  
  (display "Broadcasting command to all devices...\n")
  (egregore-broadcast device-swarm 'calibrate '((precision . high)))
  (sleep 0.2)
  (display "  ✓ All devices received calibration command\n\n")
  
  (egregore-stop device-swarm)
  
  ;; Summary
  (display "=== Summary ===\n\n")
  
  (display "D9 Address System:\n")
  (display "  • URI-based daemon addressing (d9://type/group/name)\n")
  (display "  • Direct routing without namespace lookups\n")
  (display "  • Address resolution and message forwarding\n\n")
  
  (display "Thread Pool Management:\n")
  (display "  • Instant cycle spawn/drop (1 to 10^12 potential)\n")
  (display "  • Demonstrated scaling: 1 -> 1000 -> 1 cycles\n")
  (display "  • Synchronous cycle coordination\n\n")
  
  (display "Virtual Devices:\n")
  (display "  • Gesture Device: Spatio-temporal vectors with intent\n")
  (display "  •   - Records position, velocity, direction, timestamp\n")
  (display "  •   - Recognizes intent (tap, swipe, drag)\n")
  (display "  • Clockwork Device: Nested gearing arrays\n")
  (display "  •   - Multiple interconnected gears\n")
  (display "  •   - Complex temporal orchestration\n\n")
  
  (display "Egregore-Device Integration:\n")
  (display "  • Egregores can initiate and coordinate devices\n")
  (display "  • Virtual devices as pure daemons\n")
  (display "  • Orchestrarchitecture: everything is a daemon\n\n")
  
  (display "=== Pure Daemon Orchestrarchitecture Achieved ===\n\n")
  (display "All components (routers, pools, devices, egregores) are daemons!\n\n"))

;; Run the example
(main)
