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

(define-module (guix antikythera records)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (temporal-flux-mode
            temporal-flux-state
            temporal-flux-state?
            temporal-flux-state-mode
            temporal-flux-state-chron-count
            temporal-flux-state-kairon-count
            temporal-flux-state-aion-count
            temporal-flux-state-energy
            make-temporal-flux-state

            recursion-context
            recursion-context?
            recursion-context-depth
            recursion-context-parent
            recursion-context-bindings
            recursion-context-flux-mode
            recursion-context-timestamp
            make-recursion-context

            egregore-config
            egregore-config?
            egregore-config-chron-weight
            egregore-config-kairon-weight
            egregore-config-aion-weight
            egregore-config-transition-threshold
            egregore-config-cycle-detection
            make-egregore-config

            reactor-state
            reactor-state?
            reactor-state-energy-budget
            reactor-state-chron-budget
            reactor-state-kairon-budget
            reactor-state-aion-budget
            reactor-state-state-vector
            reactor-state-event-queue
            make-reactor-state

            tensor-state
            tensor-state?
            tensor-state-shape
            tensor-state-data
            tensor-state-worker-count
            tensor-state-task-queue-depth
            tensor-state-flux-mode
            make-tensor-state))

;;; Commentary:
;;;
;;; This module defines the core record types for the AnTiKytHeRa nested
;;; recursion engine. These records represent the fundamental data structures
;;; used throughout the engine for temporal flux control, recursion management,
;;; and state tracking.
;;;
;;; Code:

;; Temporal flux modes for the triadic egregore system
(define temporal-flux-mode
  '(chron    ; Sequential, linear time progression
    kairon   ; Opportune time, optimal moments
    aion))   ; Cyclical, eternal time patterns

;; Temporal flux state - represents the current state of the triadic egregore system
;; tracking which temporal mode is active and energy levels
(define-record-type <temporal-flux-state>
  (%make-temporal-flux-state mode chron-count kairon-count aion-count energy)
  temporal-flux-state?
  (mode          temporal-flux-state-mode)         ; Current temporal mode (chron/kairon/aion)
  (chron-count   temporal-flux-state-chron-count)  ; Number of ChRoN mode activations
  (kairon-count  temporal-flux-state-kairon-count) ; Number of KAiRoN mode activations
  (aion-count    temporal-flux-state-aion-count)   ; Number of AIoN mode activations
  (energy        temporal-flux-state-energy))      ; Current energy level (0.0-1.0)

(define* (make-temporal-flux-state #:key (mode 'chron)
                                          (chron-count 0)
                                          (kairon-count 0)
                                          (aion-count 0)
                                          (energy 1.0))
  "Create a new temporal flux state with the specified mode and counters."
  (%make-temporal-flux-state mode chron-count kairon-count aion-count energy))

;; Recursion context - captures the state of a recursive computation
(define-record-type <recursion-context>
  (%make-recursion-context depth parent bindings flux-mode timestamp)
  recursion-context?
  (depth      recursion-context-depth)      ; Recursion depth (0 = root)
  (parent     recursion-context-parent)     ; Parent context (or #f for root)
  (bindings   recursion-context-bindings)   ; Association list of local bindings
  (flux-mode  recursion-context-flux-mode)  ; Temporal flux mode for this context
  (timestamp  recursion-context-timestamp)) ; When this context was created

(define* (make-recursion-context #:key (depth 0)
                                       (parent #f)
                                       (bindings '())
                                       (flux-mode 'chron)
                                       (timestamp (current-time)))
  "Create a new recursion context with the specified parameters."
  (%make-recursion-context depth parent bindings flux-mode timestamp))

;; Egregore configuration - settings for the triadic temporal flux masters
(define-record-type <egregore-config>
  (%make-egregore-config chron-weight kairon-weight aion-weight
                         transition-threshold cycle-detection)
  egregore-config?
  (chron-weight          egregore-config-chron-weight)          ; Weight for ChRoN mode (0.0-1.0)
  (kairon-weight         egregore-config-kairon-weight)         ; Weight for KAiRoN mode (0.0-1.0)
  (aion-weight           egregore-config-aion-weight)           ; Weight for AIoN mode (0.0-1.0)
  (transition-threshold  egregore-config-transition-threshold)  ; Threshold for mode transitions
  (cycle-detection       egregore-config-cycle-detection))      ; Enable cycle detection (#t/#f)

(define* (make-egregore-config #:key (chron-weight 0.4)
                                     (kairon-weight 0.3)
                                     (aion-weight 0.3)
                                     (transition-threshold 0.7)
                                     (cycle-detection #t))
  "Create a new egregore configuration with the specified weights and settings."
  (%make-egregore-config chron-weight kairon-weight aion-weight
                         transition-threshold cycle-detection))

;; Reactor state - state of the ATenCoRe reactor
(define-record-type <reactor-state>
  (%make-reactor-state energy-budget chron-budget kairon-budget aion-budget
                       state-vector event-queue)
  reactor-state?
  (energy-budget  reactor-state-energy-budget)   ; Total energy budget
  (chron-budget   reactor-state-chron-budget)    ; Energy allocated to ChRoN mode
  (kairon-budget  reactor-state-kairon-budget)   ; Energy allocated to KAiRoN mode
  (aion-budget    reactor-state-aion-budget)     ; Energy allocated to AIoN mode
  (state-vector   reactor-state-state-vector)    ; 3D state vector [chron kairon aion]
  (event-queue    reactor-state-event-queue))    ; Queue of pending events

(define* (make-reactor-state #:key (energy-budget 1000.0)
                                   (chron-budget 400.0)
                                   (kairon-budget 300.0)
                                   (aion-budget 300.0)
                                   (state-vector #(1.0 0.0 0.0))
                                   (event-queue '()))
  "Create a new reactor state with the specified energy budgets and initial state."
  (%make-reactor-state energy-budget chron-budget kairon-budget aion-budget
                       state-vector event-queue))

;; Tensor state - representation of worker pool state as a tensor
(define-record-type <tensor-state>
  (%make-tensor-state shape data worker-count task-queue-depth flux-mode)
  tensor-state?
  (shape             tensor-state-shape)             ; Tensor dimensions (e.g., '(3 3) for 3x3)
  (data              tensor-state-data)              ; Flat vector of tensor data
  (worker-count      tensor-state-worker-count)      ; Number of active workers
  (task-queue-depth  tensor-state-task-queue-depth)  ; Depth of task queue
  (flux-mode         tensor-state-flux-mode))        ; Current temporal flux mode

(define* (make-tensor-state #:key (shape '(3 3))
                                  (data (make-vector 9 0.0))
                                  (worker-count 0)
                                  (task-queue-depth 0)
                                  (flux-mode 'chron))
  "Create a new tensor state representing a worker pool state."
  (%make-tensor-state shape data worker-count task-queue-depth flux-mode))

;;; records.scm ends here
