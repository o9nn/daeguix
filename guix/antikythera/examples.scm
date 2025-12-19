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

(use-modules (guix antikythera)
             (guix antikythera records)
             (guix antikythera tensor-gears)
             (guix antikythera atencore))

;;; Commentary:
;;;
;;; Example usage demonstrations for the AnTiKytHeRa nested recursion engine.
;;; Shows how to use the three temporal flux modes (ChRoN, KAiRoN, AIoN)
;;; for different types of recursive computations.
;;;
;;; Code:

(define (display-section title)
  "Display a section header."
  (newline)
  (display "==============================================")
  (newline)
  (display title)
  (newline)
  (display "==============================================")
  (newline))

;;; Example 1: Basic Fibonacci with ChRoN mode (sequential)
(display-section "Example 1: Fibonacci with ChRoN Mode")

(define (fibonacci n)
  "Classic recursive Fibonacci."
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define engine1 (make-antikythera-engine #:worker-count 4))
(engine-start engine1)

(display "Computing fibonacci(10) with ChRoN mode (sequential)...")
(newline)
(let ((result (engine-recurse engine1 fibonacci '(10) #:mode 'chron)))
  (display "Result: ")
  (display result)
  (newline))

(display "Engine state after ChRoN recursion:")
(newline)
(display (engine-state engine1))
(newline)

;;; Example 2: Fibonacci with AIoN mode (memoized)
(display-section "Example 2: Fibonacci with AIoN Mode (Memoization)")

(display "Computing fibonacci(30) with AIoN mode (memoized)...")
(newline)
(let ((result (engine-recurse engine1 fibonacci '(30) #:mode 'aion)))
  (display "Result: ")
  (display result)
  (newline))

(display "Engine state after AIoN recursion:")
(newline)
(display (engine-state engine1))
(newline)

;;; Example 3: Tree traversal with auto mode
(display-section "Example 3: Tree Traversal with Auto Mode")

(define (tree-sum tree)
  "Sum all values in a tree structure."
  (cond
    ((null? tree) 0)
    ((number? tree) tree)
    ((pair? tree)
     (+ (tree-sum (car tree))
        (tree-sum (cdr tree))))
    (else 0)))

(define test-tree '((1 (2 3)) (4 (5 (6 7)))))

(display "Computing tree-sum with auto mode...")
(newline)
(display "Tree: ")
(display test-tree)
(newline)
(let ((result (engine-recurse engine1 tree-sum (list test-tree) #:mode 'auto)))
  (display "Sum: ")
  (display result)
  (newline))

;;; Example 4: Temporal flux mode transitions
(display-section "Example 4: Temporal Flux Mode Transitions")

(display "Initial flux mode: ")
(display (assoc-ref (engine-state engine1) 'flux-mode))
(newline)

(display "Transitioning to KAiRoN mode...")
(newline)
(set! engine1 (transition-flux-mode engine1 'kairon))
(display "New flux mode: ")
(display (assoc-ref (engine-state engine1) 'flux-mode))
(newline)

(display "Transitioning to AIoN mode...")
(newline)
(set! engine1 (transition-flux-mode engine1 'aion))
(display "New flux mode: ")
(display (assoc-ref (engine-state engine1) 'flux-mode))
(newline)

;;; Example 5: Tensor-Gears demonstration
(display-section "Example 5: Tensor-Gears Matrix Operations")

(display "Creating tensor state...")
(newline)
(define tensor (tensor-create #:shape '(3 3)))
(display "Initial tensor flux mode: ")
(display (tensor-state-flux-mode tensor))
(newline)

(display "Applying Clock Matrix (temporal progression)...")
(newline)
(define tensor-advanced (advance-temporal-phase tensor))
(display "After Clock Matrix: ")
(display (tensor-state-flux-mode tensor-advanced))
(newline)

(display "Applying Clock Matrix again...")
(newline)
(define tensor-advanced2 (advance-temporal-phase tensor-advanced))
(display "After second Clock Matrix: ")
(display (tensor-state-flux-mode tensor-advanced2))
(newline)

(display "Applying Shift Matrix...")
(newline)
(define tensor-shifted (shift-operational-mode tensor))
(display "After Shift Matrix - tensor state is transformed")
(newline)

;;; Example 6: ATenCoRe Reactor demonstration
(display-section "Example 6: ATenCoRe Reactor State Management")

(display "Initializing reactor...")
(newline)
(define reactor (reactor-init))
(display "Initial energy budget: ")
(display (reactor-state-energy-budget reactor))
(newline)
(display "Initial state vector: ")
(display (reactor-state-state-vector reactor))
(newline)

(display "Performing state transition...")
(newline)
(define reactor-transitioned 
  (reactor-state-transition reactor 'clock-advance 'chron))
(display "After transition - ChRoN budget: ")
(display (reactor-state-chron-budget reactor-transitioned))
(newline)

(display "Computing energy cost for operations...")
(newline)
(display "Clock advance cost: ")
(display (reactor-compute-energy-cost 'clock-advance reactor))
(newline)
(display "Shift mode cost: ")
(display (reactor-compute-energy-cost 'shift-mode reactor))
(newline)
(display "Recursion step cost: ")
(display (reactor-compute-energy-cost 'recursion-step reactor))
(newline)

(display "Checking energy optimization recommendations...")
(newline)
(define recommendations (reactor-optimize-energy reactor-transitioned))
(display "Recommendations: ")
(display recommendations)
(newline)

;;; Example 7: Deep recursion with KAiRoN mode
(display-section "Example 7: Ackermann Function with KAiRoN Mode")

(define (ackermann m n)
  "Ackermann function - grows very quickly."
  (cond
    ((= m 0) (+ n 1))
    ((= n 0) (ackermann (- m 1) 1))
    (else (ackermann (- m 1) (ackermann m (- n 1))))))

(display "Computing ackermann(2, 3) with KAiRoN mode (opportunistic)...")
(newline)
(let ((result (engine-recurse engine1 ackermann '(2 3) #:mode 'kairon)))
  (display "Result: ")
  (display result)
  (newline))

;;; Example 8: Cycle detection
(display-section "Example 8: Cycle Detection")

(display "Testing cycle detection with repeated arguments...")
(newline)
(define test-args '(10 20 30))
(display "First call with args: ")
(display test-args)
(newline)
(display "Cycle detected? ")
(display (detect-cycle engine1 test-args))
(newline)

;; Call once to populate cache
(engine-recurse engine1 fibonacci '(8) #:mode 'aion)

(display "Second call with same args (should detect cycle in AIoN mode)...")
(newline)
(display "Cycle detected? ")
(display (detect-cycle engine1 '(8)))
(newline)

;;; Example 9: Custom egregore configuration
(display-section "Example 9: Custom Egregore Configuration")

(define custom-config 
  (make-egregore-config
    #:chron-weight 0.5    ; 50% weight for sequential
    #:kairon-weight 0.3   ; 30% weight for opportunistic
    #:aion-weight 0.2     ; 20% weight for cyclical
    #:transition-threshold 0.8
    #:cycle-detection #t))

(display "Custom egregore configuration:")
(newline)
(display "  ChRoN weight: ")
(display (egregore-config-chron-weight custom-config))
(newline)
(display "  KAiRoN weight: ")
(display (egregore-config-kairon-weight custom-config))
(newline)
(display "  AIoN weight: ")
(display (egregore-config-aion-weight custom-config))
(newline)
(display "  Transition threshold: ")
(display (egregore-config-transition-threshold custom-config))
(newline)
(display "  Cycle detection: ")
(display (egregore-config-cycle-detection custom-config))
(newline)

(define engine2 
  (make-antikythera-engine 
    #:worker-count 8
    #:egregore-config custom-config))

(engine-start engine2)
(display "Created new engine with custom configuration")
(newline)

;;; Cleanup
(display-section "Cleanup")

(display "Stopping engines...")
(newline)
(engine-stop engine1)
(engine-stop engine2)
(display "Engines stopped. Examples complete.")
(newline)

;;; examples.scm ends here
