;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 AnTiKytHeRa Project
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

(define-module (test-antikythera-recursion)
  #:use-module (guix antikythera)
  #:use-module (guix antikythera records)
  #:use-module (guix antikythera tensor-gears)
  #:use-module (guix antikythera atencore)
  #:use-module (srfi srfi-64))

;;; Commentary:
;;;
;;; Test suite for the AnTiKytHeRa nested recursion engine.
;;; Tests record types, tensor operations, reactor functionality,
;;; and the main recursion engine with triadic temporal flux control.
;;;
;;; Code:

(test-begin "antikythera-recursion-records")

(test-assert "temporal-flux-state creation"
  (temporal-flux-state? (make-temporal-flux-state)))

(test-equal "temporal-flux-state default mode"
  'chron
  (temporal-flux-state-mode (make-temporal-flux-state)))

(test-equal "temporal-flux-state custom values"
  0.75
  (temporal-flux-state-energy 
    (make-temporal-flux-state #:energy 0.75)))

(test-assert "recursion-context creation"
  (recursion-context? (make-recursion-context)))

(test-equal "recursion-context depth"
  5
  (recursion-context-depth 
    (make-recursion-context #:depth 5)))

(test-assert "egregore-config creation"
  (egregore-config? (make-egregore-config)))

(test-equal "egregore-config weights sum to 1.0"
  1.0
  (let ((config (make-egregore-config)))
    (+ (egregore-config-chron-weight config)
       (egregore-config-kairon-weight config)
       (egregore-config-aion-weight config))))

(test-assert "reactor-state creation"
  (reactor-state? (make-reactor-state)))

(test-equal "reactor-state energy budget"
  1000.0
  (reactor-state-energy-budget (make-reactor-state)))

(test-assert "tensor-state creation"
  (tensor-state? (make-tensor-state)))

(test-equal "tensor-state shape"
  '(3 3)
  (tensor-state-shape (make-tensor-state #:shape '(3 3))))

(test-end "antikythera-recursion-records")

(test-begin "antikythera-recursion-tensor-gears")

(test-assert "sylvester-clock-matrix is defined"
  (vector? sylvester-clock-matrix))

(test-assert "sylvester-shift-matrix is defined"
  (vector? sylvester-shift-matrix))

(test-equal "clock-matrix dimensions"
  3
  (vector-length sylvester-clock-matrix))

(test-equal "shift-matrix dimensions"
  3
  (vector-length sylvester-shift-matrix))

(test-assert "tensor-create produces tensor"
  (tensor-state? (tensor-create)))

(test-assert "advance-temporal-phase changes mode"
  (let* ((tensor (make-tensor-state #:flux-mode 'chron))
         (advanced (advance-temporal-phase tensor)))
    (eq? (tensor-state-flux-mode advanced) 'kairon)))

(test-assert "shift-operational-mode applies shift matrix"
  (let* ((tensor (tensor-create))
         (shifted (shift-operational-mode tensor)))
    (tensor-state? shifted)))

(test-end "antikythera-recursion-tensor-gears")

(test-begin "antikythera-recursion-atencore")

(test-assert "reactor-init creates reactor state"
  (reactor-state? (reactor-init)))

(test-equal "reactor initial energy budget"
  1000.0
  (reactor-state-energy-budget (reactor-init)))

(test-equal "reactor initial state vector"
  #(1.0 0.0 0.0)
  (reactor-state-state-vector (reactor-init)))

(test-assert "reactor-compute-energy-cost returns number"
  (number? (reactor-compute-energy-cost 'clock-advance (reactor-init))))

(test-assert "reactor-state-transition produces new state"
  (let* ((reactor (reactor-init))
         (new-reactor (reactor-state-transition reactor 'clock-advance 'chron)))
    (reactor-state? new-reactor)))

(test-assert "reactor-allocate-budget updates budgets"
  (let* ((reactor (reactor-init))
         (realloc (reactor-allocate-budget reactor 500.0)))
    (= 500.0 (reactor-state-energy-budget realloc))))

(test-assert "reactor-optimize-energy returns recommendations"
  (list? (reactor-optimize-energy (reactor-init))))

(test-end "antikythera-recursion-atencore")

(test-begin "antikythera-recursion-engine")

(test-assert "make-antikythera-engine creates engine"
  (antikythera-engine? (make-antikythera-engine)))

(test-assert "engine-start sets running flag"
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (assoc-ref (engine-state engine) 'running)))

(test-assert "engine-stop clears running flag"
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (engine-stop engine)
    (not (assoc-ref (engine-state engine) 'running))))

(test-equal "engine-state returns alist"
  'chron
  (let ((engine (make-antikythera-engine)))
    (assoc-ref (engine-state engine) 'flux-mode)))

(test-assert "transition-flux-mode changes mode"
  (let* ((engine (make-antikythera-engine))
         (transitioned (transition-flux-mode engine 'kairon))
         (state (engine-state transitioned)))
    (eq? (assoc-ref state 'flux-mode) 'kairon)))

;; Test recursive computation
(define (test-factorial n)
  "Simple factorial function for testing recursion."
  (if (<= n 1)
      1
      (* n (test-factorial (- n 1)))))

(test-equal "engine-recurse performs computation"
  120
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (engine-recurse engine test-factorial '(5) #:mode 'chron)))

(test-equal "chron mode recursion"
  24
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (engine-recurse engine test-factorial '(4) #:mode 'chron)))

(test-equal "auto mode recursion"
  720
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (engine-recurse engine test-factorial '(6) #:mode 'auto)))

(test-assert "detect-cycle returns boolean"
  (let ((engine (make-antikythera-engine)))
    (boolean? (detect-cycle engine '(1 2 3)))))

(test-end "antikythera-recursion-engine")

;; Integration tests
(test-begin "antikythera-recursion-integration")

(test-assert "engine integrates with tensor-gears"
  (let* ((engine (make-antikythera-engine))
         (tensor (tensor-create)))
    (and (antikythera-engine? engine)
         (tensor-state? tensor))))

(test-assert "engine integrates with reactor"
  (let* ((engine (make-antikythera-engine))
         (reactor (reactor-init)))
    (and (antikythera-engine? engine)
         (reactor-state? reactor))))

(test-assert "complete workflow: create, start, recurse, stop"
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (let ((result (engine-recurse engine test-factorial '(5) #:mode 'chron)))
      (engine-stop engine)
      (= result 120))))

(test-end "antikythera-recursion-integration")

;;; test-antikythera-recursion.scm ends here
