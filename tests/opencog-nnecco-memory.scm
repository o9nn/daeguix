;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 NNECCO-A9NN Contributors
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

(define-module (test-opencog-nnecco-memory)
  #:use-module (gnu opencog nnecco-memory)
  #:use-module (gnu opencog daemons)
  #:use-module (srfi srfi-64))

(test-begin "opencog-nnecco-memory")

;;;
;;; AtomSpace Daemon Tests
;;;

(test-assert "atomspace-daemon creation"
  (let ((daemon (make-atomspace-daemon #:name "test-atomspace")))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-atomspace")
         (equal? (daemon-type daemon) 'atomspace))))

(test-assert "atomspace add node"
  (let* ((daemon (make-atomspace-daemon))
         (tv (make-truth-value 0.8 0.9))
         (atom (atomspace-add-node daemon 'ConceptNode "test-concept" tv 0.5 '())))
    (and (atom? atom)
         (equal? (atom-type atom) 'ConceptNode)
         (equal? (atom-name atom) "test-concept")
         (= (atom-attention atom) 0.5))))

(test-assert "atomspace add multiple nodes"
  (let ((daemon (make-atomspace-daemon)))
    (atomspace-add-node daemon 'ConceptNode "cat" #f #f '())
    (atomspace-add-node daemon 'ConceptNode "dog" #f #f '())
    (atomspace-add-node daemon 'ConceptNode "animal" #f #f '())
    (= (daemon-get-metric daemon 'total-atoms) 3)))

(test-assert "atomspace add link"
  (let* ((daemon (make-atomspace-daemon))
         (cat (atomspace-add-node daemon 'ConceptNode "cat" #f #f '()))
         (animal (atomspace-add-node daemon 'ConceptNode "animal" #f #f '()))
         (link (atomspace-add-link daemon 'InheritanceLink (list cat animal) #f)))
    (and (list? link)
         (equal? (assoc-ref link 'type) 'InheritanceLink))))

(test-assert "atomspace query by type"
  (let* ((daemon (make-atomspace-daemon))
         (_ (atomspace-add-node daemon 'ConceptNode "concept1" #f #f '()))
         (_ (atomspace-add-node daemon 'ConceptNode "concept2" #f #f '()))
         (_ (atomspace-add-node daemon 'PredicateNode "predicate1" #f #f '()))
         (results (atomspace-query daemon 'ConceptNode)))
    (= (length results) 2)))

(test-assert "atomspace query by name substring"
  (let* ((daemon (make-atomspace-daemon))
         (_ (atomspace-add-node daemon 'ConceptNode "neural-network" #f #f '()))
         (_ (atomspace-add-node daemon 'ConceptNode "neural-cognition" #f #f '()))
         (_ (atomspace-add-node daemon 'ConceptNode "robot" #f #f '()))
         (results (atomspace-query daemon "neural")))
    (= (length results) 2)))

(test-assert "atomspace attention spreading"
  (let* ((daemon (make-atomspace-daemon))
         (source (atomspace-add-node daemon 'ConceptNode "source" #f 1.0 '()))
         (target1 (atomspace-add-node daemon 'ConceptNode "target1" #f 0.1 '()))
         (target2 (atomspace-add-node daemon 'ConceptNode "target2" #f 0.1 '()))
         (_ (atomspace-add-link daemon 'AssociativeLink (list source target1) #f))
         (_ (atomspace-add-link daemon 'AssociativeLink (list source target2) #f))
         (attention-before (atom-attention target1)))
    
    (atomspace-spread-attention daemon source)
    
    (> (atom-attention target1) attention-before)))

;;;
;;; Episodic Memory Daemon Tests
;;;

(test-assert "episodic-memory-daemon creation"
  (let ((daemon (make-episodic-memory-daemon #:name "test-episodic"
                                             #:max-episodes 100)))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-episodic")
         (equal? (daemon-type daemon) 'episodic-memory))))

(test-assert "episodic store"
  (let* ((daemon (make-episodic-memory-daemon))
         (episode (episodic-store daemon
                                 '((location . "home"))
                                 "Had breakfast"
                                 '((happy . 0.7))
                                 '(morning routine))))
    (and (episode? episode)
         (equal? (episode-content episode) "Had breakfast")
         (member 'morning (episode-tags episode)))))

(test-assert "episodic store multiple"
  (let ((daemon (make-episodic-memory-daemon)))
    (episodic-store daemon '() "Event 1" '() '(tag1))
    (episodic-store daemon '() "Event 2" '() '(tag2))
    (episodic-store daemon '() "Event 3" '() '(tag1 tag2))
    (= (daemon-get-metric daemon 'episode-count) 3)))

(test-assert "episodic recall by content"
  (let ((daemon (make-episodic-memory-daemon)))
    (episodic-store daemon '() "Learned about neural networks" '() '(learning))
    (episodic-store daemon '() "Played a game" '() '(play))
    (episodic-store daemon '() "Neural processing study" '() '(learning))
    
    (let ((results (episodic-recall daemon "neural")))
      (= (length results) 2))))

(test-assert "episodic query by tags"
  (let ((daemon (make-episodic-memory-daemon)))
    (episodic-store daemon '() "Morning routine" '() '(morning routine))
    (episodic-store daemon '() "Evening walk" '() '(evening exercise))
    (episodic-store daemon '() "Morning exercise" '() '(morning exercise))
    
    (let ((results (episodic-query daemon '(morning) #f)))
      (>= (length results) 2))))

(test-assert "episodic query with limit"
  (let ((daemon (make-episodic-memory-daemon)))
    (episodic-store daemon '() "Event 1" '() '(test))
    (episodic-store daemon '() "Event 2" '() '(test))
    (episodic-store daemon '() "Event 3" '() '(test))
    (episodic-store daemon '() "Event 4" '() '(test))
    
    (let ((results (episodic-query daemon '(test) 2)))
      (= (length results) 2))))

(test-assert "episodic memory pruning"
  (let ((daemon (make-episodic-memory-daemon #:max-episodes 5)))
    ;; Store more than max
    (do ((i 0 (+ i 1)))
        ((>= i 10))
      (episodic-store daemon '() (format #f "Event ~a" i) '() '()))
    
    (<= (length (daemon-get-metric daemon 'episodes)) 5)))

;;;
;;; Replay Memory Daemon Tests
;;;

(test-assert "replay-memory-daemon creation"
  (let ((daemon (make-replay-memory-daemon #:name "test-replay"
                                          #:capacity 1000)))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-replay")
         (equal? (daemon-type daemon) 'replay-memory))))

(test-assert "replay store experience"
  (let* ((daemon (make-replay-memory-daemon))
         (exp (replay-store daemon
                           '((state . "s1"))
                           '((action . "a1"))
                           1.0
                           '((state . "s2"))
                           0.8)))
    (and (experience? exp)
         (= (experience-reward exp) 1.0)
         (= (experience-priority exp) 0.8))))

(test-assert "replay store multiple experiences"
  (let ((daemon (make-replay-memory-daemon)))
    (replay-store daemon '((s . 1)) '((a . 1)) 0.5 '((s . 2)) 1.0)
    (replay-store daemon '((s . 2)) '((a . 2)) 0.7 '((s . 3)) 0.9)
    (replay-store daemon '((s . 3)) '((a . 3)) -0.2 '((s . 4)) 1.2)
    (= (daemon-get-metric daemon 'experience-count) 3)))

(test-assert "replay sample batch"
  (let ((daemon (make-replay-memory-daemon)))
    ;; Store experiences
    (do ((i 0 (+ i 1)))
        ((>= i 20))
      (replay-store daemon
                   `((state . ,i))
                   `((action . ,i))
                   (random 1.0)
                   `((state . ,(+ i 1)))
                   (random 1.0)))
    
    (let ((samples (replay-sample daemon 8)))
      (= (length samples) 8))))

(test-assert "replay sample respects batch size"
  (let ((daemon (make-replay-memory-daemon)))
    (replay-store daemon '() '() 0 '() 1.0)
    (replay-store daemon '() '() 0 '() 1.0)
    (replay-store daemon '() '() 0 '() 1.0)
    
    (let ((samples (replay-sample daemon 10)))
      (<= (length samples) 3))))

(test-assert "replay prioritize experience"
  (let* ((daemon (make-replay-memory-daemon))
         (exp (replay-store daemon '() '() 0 '() 0.5))
         (exp-id (experience-id exp)))
    
    (replay-prioritize daemon exp-id 1.5)
    
    ;; Find the experience and check priority
    (let ((experiences (daemon-get-metric daemon 'experiences)))
      (let ((found (find (lambda (e) (equal? (experience-id e) exp-id))
                        experiences)))
        (and found (= (experience-priority found) 1.5))))))

(test-assert "replay memory capacity limit"
  (let ((daemon (make-replay-memory-daemon #:capacity 10)))
    ;; Store more than capacity
    (do ((i 0 (+ i 1)))
        ((>= i 15))
      (replay-store daemon '() '() 0 '() 1.0))
    
    (<= (length (daemon-get-metric daemon 'experiences)) 10)))

;;;
;;; Personality Tensor Daemon Tests
;;;

(test-assert "personality-daemon creation"
  (let ((daemon (make-personality-daemon #:name "test-personality")))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-personality")
         (equal? (daemon-type daemon) 'personality))))

(test-assert "personality default traits"
  (let* ((daemon (make-personality-daemon))
         (traits (personality-get-traits daemon)))
    (and (list? traits)
         (assoc-ref traits 'playfulness)
         (assoc-ref traits 'intelligence)
         (assoc-ref traits 'chaotic))))

(test-assert "personality set trait"
  (let ((daemon (make-personality-daemon)))
    (personality-set-trait daemon 'curiosity 0.95)
    (let ((traits (personality-get-traits daemon)))
      (= (assoc-ref traits 'curiosity) 0.95))))

(test-assert "personality trait clamping"
  (let ((daemon (make-personality-daemon)))
    (personality-set-trait daemon 'chaotic 1.5) ; Out of range
    (personality-set-trait daemon 'empathy -0.5) ; Out of range
    (let ((traits (personality-get-traits daemon)))
      (and (<= (assoc-ref traits 'chaotic) 1.0)
           (>= (assoc-ref traits 'chaotic) 0.0)
           (<= (assoc-ref traits 'empathy) 1.0)
           (>= (assoc-ref traits 'empathy) 0.0)))))

(test-assert "personality frame selection"
  (let ((daemon (make-personality-daemon)))
    (let ((frame (personality-select-frame daemon)))
      (member frame '(chaos strategy play neutral)))))

(test-assert "personality frame selection - chaos"
  (let ((daemon (make-personality-daemon)))
    (personality-set-trait daemon 'chaotic 0.95)
    (personality-set-trait daemon 'intelligence 0.5)
    (let ((frame (personality-select-frame daemon)))
      (equal? frame 'chaos))))

(test-assert "personality frame selection - strategy"
  (let ((daemon (make-personality-daemon)))
    (personality-set-trait daemon 'chaotic 0.3)
    (personality-set-trait daemon 'intelligence 0.95)
    (let ((frame (personality-select-frame daemon)))
      (equal? frame 'strategy))))

(test-assert "personality frame selection - play"
  (let ((daemon (make-personality-daemon)))
    (personality-set-trait daemon 'chaotic 0.3)
    (personality-set-trait daemon 'intelligence 0.5)
    (personality-set-trait daemon 'playfulness 0.95)
    (let ((frame (personality-select-frame daemon)))
      (equal? frame 'play))))

(test-assert "personality frame history"
  (let ((daemon (make-personality-daemon)))
    (personality-select-frame daemon)
    (personality-select-frame daemon)
    (personality-select-frame daemon)
    (let ((history (daemon-get-metric daemon 'frame-history)))
      (= (length history) 3))))

(test-assert "personality custom traits"
  (let* ((custom-traits '((creativity . 0.9)
                         (logic . 0.8)
                         (social . 0.6)))
         (daemon (make-personality-daemon #:traits custom-traits))
         (traits (personality-get-traits daemon)))
    (and (= (assoc-ref traits 'creativity) 0.9)
         (= (assoc-ref traits 'logic) 0.8)
         (= (assoc-ref traits 'social) 0.6))))

;;;
;;; Integration Tests
;;;

(test-assert "integrated memory pipeline"
  (let* ((atomspace (make-atomspace-daemon #:name "int-atomspace"))
         (episodic (make-episodic-memory-daemon #:name "int-episodic"))
         (replay (make-replay-memory-daemon #:name "int-replay"))
         (personality (make-personality-daemon #:name "int-personality")))
    
    ;; Add concept to AtomSpace
    (let ((concept (atomspace-add-node atomspace 'ConceptNode "learning" #f #f '())))
      
      ;; Store episodic memory
      (episodic-store episodic
                     '((concept . learning))
                     "Learned new pattern"
                     '((curious . 0.8))
                     '(learning cognitive))
      
      ;; Store experience in replay
      (replay-store replay
                   '((state . initial))
                   '((action . learn))
                   1.0
                   '((state . learned))
                   1.2)
      
      ;; Select frame based on personality
      (let ((frame (personality-select-frame personality)))
        
        (and (atom? concept)
             (> (daemon-get-metric episodic 'episode-count) 0)
             (> (daemon-get-metric replay 'experience-count) 0)
             (member frame '(chaos strategy play neutral)))))))

(test-assert "daemon registry integration"
  (let* ((atomspace (make-atomspace-daemon #:name "reg-atomspace"))
         (episodic (make-episodic-memory-daemon #:name "reg-episodic"))
         (replay (make-replay-memory-daemon #:name "reg-replay"))
         (personality (make-personality-daemon #:name "reg-personality")))
    
    (and (equal? (daemon-lookup "reg-atomspace") atomspace)
         (equal? (daemon-lookup "reg-episodic") episodic)
         (equal? (daemon-lookup "reg-replay") replay)
         (equal? (daemon-lookup "reg-personality") personality))))

(test-end "opencog-nnecco-memory")
