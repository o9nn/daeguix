;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 OpenCog Orchestration Contributors
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

(define-module (gnu opencog learning)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:export (make-pattern-miner
            mine-patterns
            evaluate-pattern
            make-cognitive-learner
            learn-from-experience
            make-reinforcement-engine
            update-rewards
            select-action
            make-hebbian-learner
            update-links
            get-link-strength))

;;;
;;; Machine Learning Infrastructure for OpenCog
;;;
;;; Implements pattern mining, reinforcement learning, and Hebbian
;;; learning mechanisms in pure Scheme for autonomous agent learning.
;;;

;;; Constants

(define DEFAULT-MIN-SUPPORT 0.1)
(define DEFAULT-LEARNING-RATE 0.01)
(define DEFAULT-RL-GAMMA 0.9)
(define DEFAULT-RL-ALPHA 0.1)
(define DEFAULT-RL-EPSILON 0.1)
(define DEFAULT-HEBBIAN-DECAY 0.01)
(define DEFAULT-HEBBIAN-THRESHOLD 0.5)
(define HEBBIAN-LEARNING-DELTA 0.1)

;;;
;;; Pattern Mining
;;;

(define-record-type <pattern>
  (make-pattern structure support confidence frequency metadata)
  pattern?
  (structure pattern-structure)
  (support pattern-support set-pattern-support!)
  (confidence pattern-confidence set-pattern-confidence!)
  (frequency pattern-frequency set-pattern-frequency!)
  (metadata pattern-metadata))

(define-record-type <pattern-miner>
  (make-miner-internal patterns min-support mutex)
  pattern-miner?
  (patterns miner-patterns set-miner-patterns!)
  (min-support miner-min-support)
  (mutex miner-mutex))

(define* (make-pattern-miner #:key (min-support DEFAULT-MIN-SUPPORT))
  "Create a pattern mining engine with minimum support threshold."
  (make-miner-internal
   (make-hash-table)
   min-support
   (make-mutex)))

(define (mine-patterns miner atomspace predicate)
  "Mine frequent patterns from the AtomSpace matching predicate."
  (with-mutex (miner-mutex miner)
    ;; Simplified pattern mining algorithm
    ;; In production, this would use sophisticated algorithms like FP-growth or MOSES
    (let ((candidate-patterns '())
          (discovery-time (time-second (current-time time-monotonic))))
      (format #t "[learning] Mining patterns with min-support: ~a~%"
              (miner-min-support miner))
      ;; Extract candidate patterns
      ;; This is a placeholder - actual implementation would analyze AtomSpace structure
      (for-each
       (lambda (candidate)
         (let ((support (compute-support candidate atomspace)))
           (when (>= support (miner-min-support miner))
             (let ((pattern (make-pattern candidate support 0.0 1
                                        (list 'discovered discovery-time))))
               (hash-table-set! (miner-patterns miner)
                              (pattern-key candidate)
                              pattern)
               (set! candidate-patterns (cons pattern candidate-patterns))))))
       (generate-candidates atomspace predicate))
      (format #t "[learning] Discovered ~a patterns~%" (length candidate-patterns))
      candidate-patterns)))

(define (pattern-key structure)
  "Generate a unique key for a pattern structure."
  (format #f "~s" structure))

(define (compute-support pattern atomspace)
  "Compute the support (frequency) of a pattern in the AtomSpace."
  ;; Placeholder - would count occurrences in actual AtomSpace
  (+ 0.5 (* 0.5 (random 1.0))))

(define (generate-candidates atomspace predicate)
  "Generate candidate patterns from AtomSpace."
  ;; Placeholder - would generate actual candidates from AtomSpace structure
  '((concept-a concept-b) (concept-b concept-c) (concept-a concept-c)))

(define (evaluate-pattern pattern atomspace)
  "Evaluate the quality/usefulness of a discovered pattern."
  (let ((support (pattern-support pattern))
        (frequency (pattern-frequency pattern)))
    ;; Simple evaluation metric
    (* support (log (+ 1 frequency)))))

;;;
;;; Cognitive Learning Agent
;;;

(define-record-type <cognitive-learner>
  (make-learner-internal experiences models learning-rate mutex)
  cognitive-learner?
  (experiences learner-experiences set-learner-experiences!)
  (models learner-models set-learner-models!)
  (learning-rate learner-learning-rate)
  (mutex learner-mutex))

(define* (make-cognitive-learner #:key (learning-rate DEFAULT-LEARNING-RATE))
  "Create a cognitive learning agent with specified learning rate."
  (make-learner-internal
   '()
   (make-hash-table)
   learning-rate
   (make-mutex)))

(define (learn-from-experience learner experience outcome)
  "Learn from a single experience-outcome pair."
  (with-mutex (learner-mutex learner)
    (set-learner-experiences! learner
                             (cons (cons experience outcome)
                                   (learner-experiences learner)))
    ;; Update internal models based on experience
    (update-cognitive-models learner experience outcome)
    (format #t "[learning] Learned from experience: ~a -> ~a~%"
            (car experience) outcome)))

(define (update-cognitive-models learner experience outcome)
  "Update internal cognitive models based on new experience."
  ;; Simplified model update - in practice would use more sophisticated algorithms
  (let* ((exp-key (format #f "~s" experience))
         (current-model (hash-table-ref/default (learner-models learner)
                                                exp-key
                                                0.0))
         (alpha (learner-learning-rate learner))
         (updated-model (+ current-model (* alpha (- outcome current-model)))))
    (hash-table-set! (learner-models learner) exp-key updated-model)))

;;;
;;; Reinforcement Learning Engine
;;;

(define-record-type <reinforcement-engine>
  (make-rl-internal q-table gamma alpha epsilon mutex)
  rl-engine?
  (q-table rl-q-table)
  (gamma rl-gamma)           ; Discount factor
  (alpha rl-alpha)           ; Learning rate
  (epsilon rl-epsilon set-rl-epsilon!) ; Exploration rate
  (mutex rl-mutex))

(define* (make-reinforcement-engine #:key
                                   (gamma DEFAULT-RL-GAMMA)
                                   (alpha DEFAULT-RL-ALPHA)
                                   (epsilon DEFAULT-RL-EPSILON))
  "Create a reinforcement learning engine (Q-learning variant)."
  (make-rl-internal
   (make-hash-table)
   gamma
   alpha
   epsilon
   (make-mutex)))

(define (update-rewards engine state action reward next-state)
  "Update Q-values based on state-action-reward transition."
  (with-mutex (rl-mutex engine)
    (let* ((sa-key (format #f "~s:~s" state action))
           (current-q (hash-table-ref/default (rl-q-table engine) sa-key 0.0))
           (next-max-q (max-q-value engine next-state))
           (target (+ reward (* (rl-gamma engine) next-max-q)))
           (updated-q (+ current-q
                        (* (rl-alpha engine)
                           (- target current-q)))))
      (hash-table-set! (rl-q-table engine) sa-key updated-q)
      (format #t "[RL] Updated Q(~a,~a) = ~a~%" state action updated-q)
      updated-q)))

(define (max-q-value engine state)
  "Get maximum Q-value for a given state across all actions."
  (let ((actions '(action-a action-b action-c))) ; Placeholder actions
    (apply max
           (cons 0.0
                 (map (lambda (action)
                       (hash-table-ref/default (rl-q-table engine)
                                             (format #f "~s:~s" state action)
                                             0.0))
                     actions)))))

(define (select-action engine state available-actions)
  "Select action using epsilon-greedy policy."
  (if (< (random 1.0) (rl-epsilon engine))
      ;; Explore: random action
      (list-ref available-actions (random (length available-actions)))
      ;; Exploit: best action
      (let* ((q-values (map (lambda (action)
                             (cons action
                                   (hash-table-ref/default (rl-q-table engine)
                                                         (format #f "~s:~s" state action)
                                                         0.0)))
                           available-actions))
             (best-action (car (sort q-values
                                   (lambda (a b) (> (cdr a) (cdr b)))))))
        (format #t "[RL] Selected action: ~a~%" (car best-action))
        (car best-action))))

;;;
;;; Hebbian Learning (Neural Plasticity)
;;;

(define-record-type <hebbian-learner>
  (make-hebbian-internal links decay-rate threshold mutex)
  hebbian-learner?
  (links hebbian-links)           ; Association strengths
  (decay-rate hebbian-decay)      ; Forgetting rate
  (threshold hebbian-threshold)   ; Activation threshold
  (mutex hebbian-mutex))

(define* (make-hebbian-learner #:key
                              (decay-rate DEFAULT-HEBBIAN-DECAY)
                              (threshold DEFAULT-HEBBIAN-THRESHOLD))
  "Create a Hebbian learning system for association learning."
  (make-hebbian-internal
   (make-hash-table)
   decay-rate
   threshold
   (make-mutex)))

(define (update-links learner concept-a concept-b activation)
  "Update link strength between two concepts (Hebb's rule: neurons that fire together wire together)."
  (with-mutex (hebbian-mutex learner)
    (let* ((link-key (format #f "~s:~s" concept-a concept-b))
           (current-strength (hash-table-ref/default (hebbian-links learner)
                                                    link-key
                                                    0.0))
           ;; Hebbian update: strengthen if both active
           (delta (* activation HEBBIAN-LEARNING-DELTA))
           (new-strength (+ current-strength delta)))
      (hash-table-set! (hebbian-links learner) link-key new-strength)
      (format #t "[Hebbian] Link ~a <-> ~a strength: ~a~%"
              concept-a concept-b new-strength)
      new-strength)))

(define (get-link-strength learner concept-a concept-b)
  "Get the association strength between two concepts."
  (with-mutex (hebbian-mutex learner)
    (hash-table-ref/default (hebbian-links learner)
                          (format #f "~s:~s" concept-a concept-b)
                          0.0)))

(define (apply-decay learner)
  "Apply decay to all links (forgetting)."
  (with-mutex (hebbian-mutex learner)
    (hash-table-walk (hebbian-links learner)
                    (lambda (key strength)
                      (let ((decayed (* strength (- 1.0 (hebbian-decay learner)))))
                        (hash-table-set! (hebbian-links learner) key decayed))))))
