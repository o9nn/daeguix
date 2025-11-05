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

(define-module (test-opencog-learning)
  #:use-module (gnu opencog learning)
  #:use-module (srfi srfi-64))

(test-begin "opencog-learning")

;; Pattern Mining Tests
(test-assert "pattern-miner creation"
  (let ((miner (make-pattern-miner #:min-support 0.1)))
    (pattern-miner? miner)))

(test-assert "pattern evaluation"
  (let* ((miner (make-pattern-miner))
         (pattern (make-pattern '(concept-a concept-b) 0.8 0.9 10 '())))
    (number? (evaluate-pattern pattern 'dummy-atomspace))))

;; Cognitive Learning Tests
(test-assert "cognitive-learner creation"
  (let ((learner (make-cognitive-learner #:learning-rate 0.01)))
    (cognitive-learner? learner)))

(test-assert "learning from experience"
  (let ((learner (make-cognitive-learner)))
    (learn-from-experience learner '(state-a action-b) 0.5)
    #t))

;; Reinforcement Learning Tests
(test-assert "reinforcement-engine creation"
  (let ((engine (make-reinforcement-engine #:gamma 0.9 #:alpha 0.1)))
    (rl-engine? engine)))

(test-assert "q-value updates"
  (let ((engine (make-reinforcement-engine)))
    (number? (update-rewards engine 'state-a 'action-1 1.0 'state-b))))

(test-assert "action selection"
  (let ((engine (make-reinforcement-engine)))
    (update-rewards engine 'state-a 'action-1 1.0 'state-b)
    (update-rewards engine 'state-a 'action-2 0.5 'state-b)
    (let ((action (select-action engine 'state-a '(action-1 action-2))))
      (or (eq? action 'action-1) (eq? action 'action-2)))))

;; Hebbian Learning Tests
(test-assert "hebbian-learner creation"
  (let ((learner (make-hebbian-learner #:decay-rate 0.01)))
    (hebbian-learner? learner)))

(test-assert "link strength updates"
  (let ((learner (make-hebbian-learner)))
    (update-links learner 'concept-a 'concept-b 0.8)
    (number? (get-link-strength learner 'concept-a 'concept-b))))

(test-assert "link decay"
  (let ((learner (make-hebbian-learner)))
    (update-links learner 'concept-a 'concept-b 1.0)
    (apply-decay learner)
    (< (get-link-strength learner 'concept-a 'concept-b) 1.0)))

(test-end "opencog-learning")
