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

(define-module (test-antikythera)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 antikythera)
  #:use-module (srfi srfi-64))

;;;
;;; Tests for Dan9 Antikythera scheduler
;;;

(test-begin "antikythera")

;;;
;;; Gear tests
;;;

(test-assert "create gear"
  (let ((gear (make-gear "test-gear" 10)))
    (and (gear? gear)
         (equal? (gear-name gear) "test-gear")
         (= (gear-ratio gear) 10))))

(test-assert "create gear with parent"
  (let* ((parent (make-gear "parent" 5))
         (child (make-gear "child" 10 #:parent-gear parent)))
    (and (gear? child)
         (eq? (gear-parent child) parent))))

(test-assert "register event on gear"
  (let ((gear (make-gear "test-gear" 1))
        (fired #f))
    (register-event! gear "test-event" 1
                    (lambda (g tick)
                      (set! fired #t)))
    (and (= 1 (length (gear-events gear)))
         (equal? (event-name (car (gear-events gear))) "test-event"))))

;;;
;;; Time scaling tests
;;;

(test-assert "years to days conversion"
  (= (years->days 1) 365))

(test-assert "days to hours conversion"
  (= (days->hours 1) 24))

(test-assert "hours to minutes conversion"
  (= (hours->minutes 1) 60))

(test-assert "create time scale"
  (let ((scale (make-time-scale 'year->day 365 'day)))
    (and (time-scale? scale)
         (= (time-scale-ratio scale) 365))))

(test-assert "scale time"
  (let ((scale (make-time-scale 'year->day 365 'day)))
    (= (scale-time 2 scale) 730)))

;;;
;;; Antikythera daemon tests
;;;

(test-assert "create antikythera daemon"
  (let ((daemon (make-antikythera-daemon #:name "test-antikythera")))
    (and (daemon? daemon)
         (equal? (daemon-type daemon) 'antikythera))))

(test-assert "antikythera daemon config"
  (let ((daemon (make-antikythera-daemon #:base-tick-ms 50)))
    (= (assoc-ref (daemon-config daemon) 'base-tick-ms) 50)))

(test-assert "start and stop antikythera"
  (let ((daemon (make-antikythera-daemon #:name "test-lifecycle")))
    (antikythera-start daemon)
    (sleep 0.1)
    (let ((running (eq? 'running (daemon-state daemon))))
      (antikythera-stop daemon)
      running)))

;;;
;;; Gear ticking tests
;;;

(test-assert "gear tick count increases"
  (let ((daemon (make-antikythera-daemon #:base-tick-ms 50))
        (gear (make-gear "tick-test" 1)))
    (daemon-start daemon antikythera-daemon-loop)
    (daemon-send-message daemon daemon 'register-gear
                        `((gear . ,gear)))
    (sleep 0.3)
    (daemon-stop daemon)
    (> (gear-tick-count gear) 0)))

(test-assert "event fires on gear"
  (let ((daemon (make-antikythera-daemon #:base-tick-ms 50))
        (gear (make-gear "event-test" 1))
        (fired #f))
    (register-event! gear "fire-test" 2
                    (lambda (g tick)
                      (set! fired #t)))
    (daemon-start daemon antikythera-daemon-loop)
    (daemon-send-message daemon daemon 'register-gear
                        `((gear . ,gear)))
    (sleep 0.3)
    (daemon-stop daemon)
    fired))

;;;
;;; Nested gears test
;;;

(test-assert "nested gears with parent relationship"
  (let* ((parent (make-gear "parent" 5))
         (child (make-gear "child" 10 #:parent-gear parent)))
    (and (eq? (gear-parent child) parent)
         (not (gear-parent parent)))))

;;;
;;; Antikythera status tests
;;;

(test-assert "antikythera status"
  (let ((daemon (make-antikythera-daemon #:name "status-test"))
        (gear (make-gear "test-gear" 1)))
    (daemon-start daemon antikythera-daemon-loop)
    (sleep 0.1)
    (daemon-send-message daemon daemon 'register-gear
                        `((gear . ,gear)))
    (sleep 0.1)
    (let ((status (antikythera-status daemon)))
      (daemon-stop daemon)
      (and (assoc-ref status 'state)
           (assoc-ref status 'total-ticks)
           (assoc-ref status 'gears)))))

(test-end "antikythera")
