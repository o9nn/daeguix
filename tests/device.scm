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

(define-module (test-device)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 device)
  #:use-module (gnu dan9 antikythera)
  #:use-module (srfi srfi-64))

;;;
;;; Tests for D9 virtual devices
;;;

(test-begin "device")

;;;
;;; Virtual device tests
;;;

(test-assert "create virtual device"
  (let ((device (make-virtual-device "test-device" 'test)))
    (and (virtual-device? device)
         (equal? (device-name device) "test-device")
         (equal? (device-type device) 'test))))

;;;
;;; Gesture device tests
;;;

(test-assert "create gesture device"
  (let ((device (make-gesture-device "touchpad")))
    (and (virtual-device? device)
         (equal? (device-type device) 'gesture))))

(test-assert "create gesture vector"
  (let ((vec (make-gesture-vector 100 200 #:z 5 #:velocity 10)))
    (and (gesture-vector? vec)
         (= (gesture-x vec) 100)
         (= (gesture-y vec) 200)
         (= (gesture-z vec) 5)
         (= (gesture-velocity vec) 10))))

(test-assert "gesture record"
  (let ((device (make-gesture-device "touchpad")))
    (let ((vec (gesture-record! device 50 60 1 5 '(1 0))))
      (and (gesture-vector? vec)
           (= (gesture-x vec) 50)
           (= (gesture-y vec) 60)))))

(test-assert "gesture recognize"
  (let ((device (make-gesture-device "touchpad")))
    ;; Record some gestures
    (gesture-record! device 0 0 1 1 '(1 0))
    (gesture-record! device 10 0 1 1 '(1 0))
    (gesture-record! device 20 0 1 1 '(1 0))
    ;; Recognize should return something
    (symbol? (gesture-recognize device))))

;;;
;;; Clockwork device tests
;;;

(test-assert "create clockwork device"
  (let ((device (make-clockwork-device "mechanism")))
    (and (virtual-device? device)
         (equal? (device-type device) 'clockwork))))

(test-assert "clockwork add gear"
  (let ((device (make-clockwork-device "mechanism"))
        (gear (make-gear "test-gear" 10)))
    (clockwork-add-gear! device gear)
    #t))

(test-assert "clockwork tick"
  (let ((device (make-clockwork-device "mechanism"))
        (gear (make-gear "test-gear" 10)))
    (clockwork-add-gear! device gear)
    (let ((tick-count (clockwork-tick! device)))
      (and (number? tick-count)
           (> tick-count 0)))))

;;;
;;; Device driver tests
;;;

(test-assert "create device driver"
  (let ((driver (make-device-driver "test-driver"
                                    (lambda (device event) #t))))
    (device-driver? driver)))

(test-assert "driver attach to device"
  (let ((driver (make-device-driver "test-driver"
                                    (lambda (device event) #t)))
        (device (make-virtual-device "test-device" 'test)))
    (driver-attach! driver device)
    (eq? (device-driver device) driver)))

(test-end "device")
