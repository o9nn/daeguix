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

(define-module (gnu dan9 antikythera)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-antikythera-daemon
            antikythera-daemon-loop
            make-gear
            gear?
            gear-name
            gear-ratio
            register-event!
            antikythera-start
            antikythera-stop
            antikythera-status
            
            ;; Time scaling utilities
            make-time-scale
            time-scale?
            scale-time
            years->days
            days->hours
            hours->minutes))

;;;
;;; Antikythera Mechanism: Nested Event Loop Scheduler
;;;
;;; Inspired by the ancient Antikythera mechanism, this scheduler implements
;;; nested event loops with configurable time scaling. Like the gears of the
;;; mechanism, different event loops can run at different speeds.
;;;
;;; Example: Scale years to days, days to hours, hours to minutes
;;;
;;; This allows simulating long-term processes (like astronomical cycles) in
;;; compressed time scales.
;;;

;;; Gear record - represents an event loop at a specific time scale

(define-record-type <gear>
  (make-gear-internal name ratio parent-gear events mutex tick-count)
  gear?
  (name gear-name)
  (ratio gear-ratio)                    ; Ratio to parent gear (e.g., 365 for year->day)
  (parent-gear gear-parent set-gear-parent!)
  (events gear-events set-gear-events!) ; List of scheduled events
  (mutex gear-mutex)
  (tick-count gear-tick-count set-gear-tick-count!))

;;; Event record - scheduled events on a gear

(define-record-type <scheduled-event>
  (make-scheduled-event name tick-interval callback last-tick)
  scheduled-event?
  (name event-name)
  (tick-interval event-tick-interval)   ; Fire every N ticks
  (callback event-callback)             ; Function to call
  (last-tick event-last-tick set-event-last-tick!))

;;; Time scale record - defines time scaling ratios

(define-record-type <time-scale>
  (make-time-scale-internal name ratio base-unit)
  time-scale?
  (name time-scale-name)
  (ratio time-scale-ratio)              ; Scaling ratio
  (base-unit time-scale-base-unit))     ; Base time unit

;;;
;;; Gear Creation and Management
;;;

(define* (make-gear name ratio #:key (parent-gear #f))
  "Create a gear with NAME and time RATIO relative to parent.
RATIO defines how many ticks of this gear equal one tick of parent.
For example, ratio=365 means 365 ticks here = 1 tick in parent."
  (make-gear-internal name ratio parent-gear '() (make-mutex) 0))

(define (register-event! gear event-name tick-interval callback)
  "Register an event on GEAR that fires every TICK-INTERVAL ticks."
  (with-mutex (gear-mutex gear)
    (let ((event (make-scheduled-event event-name tick-interval callback 0)))
      (set-gear-events! gear (cons event (gear-events gear)))
      (format #t "[Gear:~a] Registered event: ~a (interval: ~a)~%"
              (gear-name gear) event-name tick-interval))))

;;;
;;; Antikythera Daemon Creation
;;;

(define* (make-antikythera-daemon #:key
                                  (name "antikythera")
                                  (base-tick-ms 100))
  "Create an Antikythera scheduler daemon.
BASE-TICK-MS is the base tick interval in milliseconds."
  (make-daemon name 'antikythera
               #:config (list (cons 'base-tick-ms base-tick-ms)
                            (cons 'gears (make-hash-table))
                            (cons 'running #f))
               #:loop-fn antikythera-daemon-loop))

;;;
;;; Main Antikythera Loop
;;;

(define (antikythera-daemon-loop daemon)
  "Main loop for the Antikythera scheduler daemon."
  (let ((base-tick-ms (assoc-ref (daemon-config daemon) 'base-tick-ms)))
    (let loop ()
      (when (eq? 'running (daemon-state daemon))
        ;; Process any incoming control messages
        (let ((message (daemon-receive-message daemon)))
          (when message
            (handle-antikythera-message daemon message)))
        
        ;; Tick all registered gears
        (tick-all-gears daemon)
        
        ;; Update metrics
        (daemon-set-metric! daemon 'total-ticks
                           (+ 1 (or (daemon-get-metric daemon 'total-ticks) 0)))
        (daemon-set-metric! daemon 'last-tick (current-time time-monotonic))
        
        ;; Sleep for base tick interval
        (usleep (* 1000 base-tick-ms))
        (loop)))))

(define (handle-antikythera-message daemon message)
  "Handle control messages for the Antikythera daemon."
  (match (message-type message)
    ('register-gear (handle-register-gear daemon message))
    ('unregister-gear (handle-unregister-gear daemon message))
    ('status (handle-status-request daemon message))
    (_ (format #t "[Antikythera] Unknown message type: ~a~%"
               (message-type message)))))

(define (handle-register-gear daemon message)
  "Handle gear registration."
  (let* ((payload (message-payload message))
         (gear (assoc-ref payload 'gear))
         (gears-table (assoc-ref (daemon-config daemon) 'gears)))
    (hash-set! gears-table (gear-name gear) gear)
    (format #t "[Antikythera] Registered gear: ~a (ratio: ~a)~%"
            (gear-name gear) (gear-ratio gear))))

(define (handle-unregister-gear daemon message)
  "Handle gear unregistration."
  (let* ((payload (message-payload message))
         (gear-name (assoc-ref payload 'gear-name))
         (gears-table (assoc-ref (daemon-config daemon) 'gears)))
    (hash-remove! gears-table gear-name)
    (format #t "[Antikythera] Unregistered gear: ~a~%" gear-name)))

(define (handle-status-request daemon message)
  "Handle status request."
  (let* ((gears-table (assoc-ref (daemon-config daemon) 'gears))
         (gear-count (hash-count (const #t) gears-table)))
    (format #t "[Antikythera] Status: ~a gears, ~a total ticks~%"
            gear-count
            (or (daemon-get-metric daemon 'total-ticks) 0))))

;;;
;;; Gear Ticking and Event Dispatch
;;;

(define (tick-all-gears daemon)
  "Tick all registered gears and fire their events."
  (let ((gears-table (assoc-ref (daemon-config daemon) 'gears)))
    (hash-for-each
     (lambda (name gear)
       (tick-gear gear daemon))
     gears-table)))

(define (tick-gear gear daemon)
  "Tick a gear and fire any due events."
  (with-mutex (gear-mutex gear)
    ;; Increment tick count
    (let ((new-tick (+ 1 (gear-tick-count gear))))
      (set-gear-tick-count! gear new-tick)
      
      ;; Check if parent gear should tick
      (when (gear-parent gear)
        (let ((ratio (gear-ratio gear)))
          (when (= 0 (modulo new-tick ratio))
            ;; This gear completed a full cycle, tick parent
            (tick-gear (gear-parent gear) daemon))))
      
      ;; Fire events that are due
      (for-each
       (lambda (event)
         (fire-event-if-due event gear daemon))
       (gear-events gear)))))

(define (fire-event-if-due event gear daemon)
  "Fire an event if it's due based on tick count."
  (let* ((current-tick (gear-tick-count gear))
         (interval (event-tick-interval event))
         (last-tick (event-last-tick event)))
    (when (>= (- current-tick last-tick) interval)
      ;; Event is due, fire it
      (catch #t
        (lambda ()
          ((event-callback event) gear current-tick))
        (lambda (key . args)
          (format #t "[Antikythera:~a] Error in event ~a: ~a ~a~%"
                  (gear-name gear) (event-name event) key args)))
      ;; Update last tick
      (set-event-last-tick! event current-tick))))

;;;
;;; Convenience Functions
;;;

(define (antikythera-start daemon)
  "Start the Antikythera scheduler."
  (daemon-start daemon antikythera-daemon-loop))

(define (antikythera-stop daemon)
  "Stop the Antikythera scheduler."
  (daemon-stop daemon))

(define (antikythera-status daemon)
  "Get status of the Antikythera scheduler."
  (let* ((gears-table (assoc-ref (daemon-config daemon) 'gears))
         (gear-list (hash-map->list
                     (lambda (k v)
                       (list (cons 'name (gear-name v))
                             (cons 'ratio (gear-ratio v))
                             (cons 'ticks (gear-tick-count v))
                             (cons 'events (length (gear-events v)))))
                     gears-table)))
    (list (cons 'state (daemon-state daemon))
          (cons 'total-ticks (or (daemon-get-metric daemon 'total-ticks) 0))
          (cons 'gears gear-list))))

;;;
;;; Time Scaling Utilities
;;;

(define (make-time-scale name ratio base-unit)
  "Create a time scale definition.
Example: (make-time-scale 'year->day 365 'day)"
  (make-time-scale-internal name ratio base-unit))

(define (scale-time value time-scale)
  "Scale a time value using the given time scale."
  (* value (time-scale-ratio time-scale)))

;;; Pre-defined time scaling functions

(define (years->days years)
  "Convert years to days (1 year = 365 days)."
  (* years 365))

(define (days->hours days)
  "Convert days to hours (1 day = 24 hours)."
  (* days 24))

(define (hours->minutes hours)
  "Convert hours to minutes (1 hour = 60 minutes)."
  (* hours 60))

;;;
;;; Helper function to create nested gear hierarchy
;;;

(define (create-nested-gears base-name ratios)
  "Create a nested hierarchy of gears with given ratios.
Example: (create-nested-gears 'time '(365 24 60))
Creates: year-gear (365) -> day-gear (24) -> hour-gear (60) -> minute-gear"
  (let loop ((remaining ratios)
             (parent #f)
             (depth 0)
             (gears '()))
    (if (null? remaining)
        (reverse gears)
        (let* ((ratio (car remaining))
               (gear (make-gear
                      (string->symbol
                       (string-append (symbol->string base-name)
                                    "-"
                                    (number->string depth)))
                      ratio
                      #:parent-gear parent)))
          (loop (cdr remaining)
                gear
                (+ depth 1)
                (cons gear gears))))))
