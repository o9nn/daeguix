#!/usr/bin/env -S guile -L ../..
!#
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

(use-modules (gnu opencog nnecco)
             (gnu opencog nnecco-memory)
             (gnu opencog daemons)
             (ice-9 threads))

(format #t "~%")
(format #t "╔════════════════════════════════════════════════════════════╗~%")
(format #t "║  NNECCO-A9NN: Complete Cognitive Architecture              ║~%")
(format #t "║                                                            ║~%")
(format #t "║  Full System Integration Example                           ║~%")
(format #t "║  with Memory, Personality, and AtomSpace                   ║~%")
(format #t "╚════════════════════════════════════════════════════════════╝~%")
(format #t "~%")

;;;
;;; Step 1: Create All Cognitive Daemons
;;;

(format #t "🔧 Creating Complete NNECCO System...~%~%")

;; Core components
(define esrp (make-esrp-daemon #:name "esrp-main" #:reservoir-size 847))
(define clp (make-clp-daemon #:name "clp-main"))
(define epu (make-epu-daemon #:name "epu-main"))
(define llama (make-llama-orchestrator-daemon #:name "llama-main" #:num-instances 4))

(format #t "  ✓ Core components created~%")

;; Memory systems
(define atomspace (make-atomspace-daemon #:name "atomspace-main"))
(define episodic (make-episodic-memory-daemon #:name "episodic-main" #:max-episodes 1000))
(define replay (make-replay-memory-daemon #:name "replay-main" #:capacity 10000))

(format #t "  ✓ Memory systems created~%")

;; Personality
(define personality (make-personality-daemon
                     #:name "personality-main"
                     #:traits '((playfulness . 0.8)
                               (intelligence . 0.9)
                               (chaotic . 0.7)
                               (empathy . 0.6)
                               (curiosity . 0.9))))

(format #t "  ✓ Personality system created~%")

;; Main NNECCO agent
(define nnecco (make-nnecco-agent-daemon
                #:name "nnecco-main"
                #:esrp-daemon esrp
                #:clp-daemon clp
                #:epu-daemon epu
                #:llama-daemon llama))

(format #t "  ✓ NNECCO orchestrator created~%~%")

;;;
;;; Step 2: Start All Daemons
;;;

(format #t "🚀 Starting All Daemon Services...~%~%")

(daemon-start esrp esrp-daemon-loop)
(daemon-start clp clp-daemon-loop)
(daemon-start epu epu-daemon-loop)
(daemon-start llama llama-daemon-loop)
(daemon-start atomspace atomspace-daemon-loop)
(daemon-start episodic episodic-memory-daemon-loop)
(daemon-start replay replay-memory-daemon-loop)
(daemon-start personality personality-daemon-loop)
(daemon-start nnecco nnecco-daemon-loop)

(format #t "  ✓ All 9 daemons running~%~%")

(sleep 1)

;;;
;;; Step 3: Build Knowledge in AtomSpace
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Building Knowledge Graph in AtomSpace~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(define nn-concept (atomspace-add-node atomspace 'ConceptNode "neural-network"
                                       (make-truth-value 0.9 0.8) 0.8 '()))
(define cog-concept (atomspace-add-node atomspace 'ConceptNode "cognition"
                                        (make-truth-value 0.85 0.9) 0.7 '()))
(define learn-concept (atomspace-add-node atomspace 'ConceptNode "learning"
                                          (make-truth-value 0.9 0.85) 0.9 '()))
(define memory-concept (atomspace-add-node atomspace 'ConceptNode "memory"
                                           (make-truth-value 0.88 0.87) 0.6 '()))

(atomspace-add-link atomspace 'InheritanceLink (list nn-concept cog-concept)
                   (make-truth-value 0.95 0.9))
(atomspace-add-link atomspace 'AssociativeLink (list learn-concept memory-concept)
                   (make-truth-value 0.9 0.85))

(format #t "  ✓ Added 4 concept nodes~%")
(format #t "  ✓ Added 2 links~%")
(format #t "  ✓ Total atoms: ~a~%~%"
        (daemon-get-metric atomspace 'total-atoms))

;;;
;;; Step 4: Store Episodic Memories
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Recording Episodic Experiences~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(episodic-store episodic
               '((location . "lab") (time . "morning"))
               "Studied neural network architectures"
               '((curious . 0.9) (focused . 0.8))
               '(learning research neural-networks))

(episodic-store episodic
               '((location . "home") (time . "evening"))
               "Implemented reservoir computing daemon"
               '((satisfied . 0.7) (creative . 0.8))
               '(coding implementation daemons))

(episodic-store episodic
               '((location . "lab") (time . "afternoon"))
               "Discovered attention spreading mechanism"
               '((excited . 0.9) (playful . 0.6))
               '(discovery cognitive-science))

(format #t "  ✓ Stored 3 episodic memories~%")
(format #t "  ✓ Total episodes: ~a~%~%"
        (daemon-get-metric episodic 'episode-count))

;;;
;;; Step 5: Store Experiences in Replay Memory
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Building Experience Replay Buffer~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(replay-store replay
             '((state . initial) (understanding . 0.3))
             '((action . read-paper))
             0.5
             '((state . learning) (understanding . 0.5))
             0.8)

(replay-store replay
             '((state . learning) (understanding . 0.5))
             '((action . implement-code))
             0.8
             '((state . understanding) (understanding . 0.7))
             1.2)

(replay-store replay
             '((state . understanding) (understanding . 0.7))
             '((action . test-system))
             1.0
             '((state . mastered) (understanding . 0.9))
             1.5)

(format #t "  ✓ Stored 3 learning experiences~%")
(format #t "  ✓ Total experiences: ~a~%~%"
        (daemon-get-metric replay 'experience-count))

;;;
;;; Step 6: Personality-Driven Behavior
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Personality-Driven Frame Selection~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(let ((traits (personality-get-traits personality)))
  (format #t "Current Personality Traits:~%")
  (for-each
   (lambda (trait)
     (format #t "  • ~a: ~,2f~%" (car trait) (cdr trait)))
   traits)
  (format #t "~%"))

(let ((frame1 (personality-select-frame personality)))
  (format #t "Selected frame (current traits): ~a~%" frame1))

;; Adjust traits and see frame change
(personality-set-trait personality 'chaotic 0.95)
(let ((frame2 (personality-select-frame personality)))
  (format #t "Selected frame (high chaos): ~a~%" frame2))

(personality-set-trait personality 'chaotic 0.3)
(personality-set-trait personality 'intelligence 0.95)
(let ((frame3 (personality-select-frame personality)))
  (format #t "Selected frame (high intelligence): ~a~%~%" frame3))

;;;
;;; Step 7: Integrated Cognitive Cycle
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Full Cognitive Cycle Integration~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(format #t "Scenario: Learning a new concept~%~%")

;; 1. Set emotional state
(epu-set-emotion epu 'curious 0.85 0.7)
(format #t "1. Emotional State: curious (0.85 intensity, 0.7 valence)~%")

;; 2. Select frame based on personality
(let ((frame (personality-select-frame personality)))
  (format #t "2. Cognitive Frame: ~a~%" frame)
  
  ;; 3. Process through consciousness layers
  (clp-process-frame clp frame "new learning opportunity" #f)
  (let ((layer (clp-get-layer clp)))
    (format #t "3. Consciousness Level: L~a (~a)~%"
            (layer-level layer) (layer-type layer)))
  
  ;; 4. Modulate reservoir based on emotion
  (let ((modulation (epu-modulate-reservoir epu frame)))
    (esrp-adapt-parameters esrp
                          (assoc-ref modulation 'input-scale-modifier)
                          frame)
    (format #t "4. Reservoir Modulated: exploration=~,3f~%"
            (assoc-ref modulation 'exploration-bonus)))
  
  ;; 5. Process through reservoir
  (let ((output (esrp-forward esrp #(0.7 0.8 0.6 0.9))))
    (format #t "5. Reservoir Output: ~a dimensions~%" (vector-length output)))
  
  ;; 6. Store in AtomSpace
  (let ((new-concept (atomspace-add-node atomspace 'ConceptNode "new-learning"
                                        (make-truth-value 0.75 0.6) 0.5 '())))
    (atomspace-add-link atomspace 'AssociativeLink
                       (list new-concept learn-concept)
                       (make-truth-value 0.8 0.7))
    (format #t "6. Stored in AtomSpace: new-learning concept~%"))
  
  ;; 7. Create episodic memory
  (episodic-store episodic
                 `((frame . ,frame))
                 "Learned through integrated cognitive cycle"
                 `((curious . 0.85))
                 '(meta-learning integration))
  (format #t "7. Episodic Memory: learning event stored~%")
  
  ;; 8. Store experience in replay
  (replay-store replay
               '((state . processing))
               '((action . integrate))
               1.2
               '((state . integrated))
               1.5)
  (format #t "8. Replay Memory: experience buffered~%"))

(format #t "~%✓ Complete cognitive cycle executed~%")

;;;
;;; Step 8: Query and Recall
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Memory Recall and Query~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

;; Query AtomSpace
(let ((results (atomspace-query atomspace "learning")))
  (format #t "AtomSpace query 'learning':~%")
  (format #t "  Found ~a related concepts~%~%" (length results)))

;; Recall episodic memories
(let ((episodes (episodic-recall episodic "learning")))
  (format #t "Episodic recall 'learning':~%")
  (format #t "  Recalled ~a episodes~%~%" (length episodes)))

;; Query by tags
(let ((tagged (episodic-query episodic '(learning research) 5)))
  (format #t "Episodic query by tags [learning, research]:~%")
  (format #t "  Found ~a episodes~%~%" (length tagged)))

;; Sample from replay
(let ((batch (replay-sample replay 3)))
  (format #t "Replay memory sample (batch=3):~%")
  (format #t "  Sampled ~a experiences~%~%" (length batch)))

;;;
;;; Step 9: Attention Spreading
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Attention Spreading in Knowledge Graph~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(format #t "Before attention spreading:~%")
(format #t "  neural-network attention: ~,3f~%" (atom-attention nn-concept))
(format #t "  cognition attention: ~,3f~%" (atom-attention cog-concept))
(format #t "  learning attention: ~,3f~%" (atom-attention learn-concept))

(atomspace-spread-attention atomspace nn-concept)

(format #t "~%After spreading from neural-network:~%")
(format #t "  neural-network attention: ~,3f~%" (atom-attention nn-concept))
(format #t "  cognition attention: ~,3f~%" (atom-attention cog-concept))
(format #t "  learning attention: ~,3f~%~%" (atom-attention learn-concept))

;;;
;;; Step 10: EchoBeats with Full Integration
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  EchoBeats Cycle with Integrated Memory~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(format #t "Running 2 complete EchoBeats cycles with memory integration...~%~%")

(do ((cycle 1 (+ cycle 1)))
    ((> cycle 2))
  (format #t "═══ Cycle ~a ═══~%" cycle)
  
  (do ((phase 1 (+ phase 1)))
      ((> phase 12))
    (nnecco-echobeat nnecco)
    
    ;; On specific phases, interact with memory systems
    (cond
     ((= phase 3) ; REPRESENT phase
      (let ((output (esrp-forward esrp #(0.5 0.6 0.7))))
        (format #t "  → Reservoir state updated~%")))
     
     ((= phase 9) ; LEARN phase
      (replay-store replay '((s . learning)) '((a . adapt)) 0.8 '((s . learned)) 1.0)
      (format #t "  → Experience stored in replay~%"))
     
     ((= phase 10) ; CONSOLIDATE phase
      (episodic-store episodic '() (format #f "Cycle ~a consolidation" cycle)
                     '((focused . 0.7)) '(echobeats))
      (format #t "  → Episode consolidated~%")))
    
    (sleep 0.15))
  
  (format #t "~%"))

;;;
;;; Step 11: System Status Report
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Complete System Status~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(let ((status (nnecco-get-hardware-status nnecco)))
  (format #t "NNECCO Hardware Registers:~%")
  (let ((registers (assoc-ref status 'registers)))
    (for-each
     (lambda (reg)
       (format #t "  ~a: ~a~%" (car reg) (cdr reg)))
     registers))
  (format #t "~%"))

(format #t "Memory Systems:~%")
(format #t "  AtomSpace: ~a atoms~%"
        (daemon-get-metric atomspace 'total-atoms))
(format #t "  Episodic Memory: ~a episodes~%"
        (daemon-get-metric episodic 'episode-count))
(format #t "  Replay Buffer: ~a experiences~%~%"
        (daemon-get-metric replay 'experience-count))

(format #t "Personality:~%")
(format #t "  Current frame: ~a~%"
        (daemon-get-metric personality 'current-frame))
(format #t "  Frame history: ~a transitions~%~%"
        (length (daemon-get-metric personality 'frame-history)))

(format #t "Daemon Registry:~%")
(format #t "  Total registered: ~a daemons~%~%"
        (length (daemon-list-all)))

;;;
;;; Step 12: Graceful Shutdown
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Graceful Shutdown~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(daemon-stop nnecco)
(daemon-stop personality)
(daemon-stop replay)
(daemon-stop episodic)
(daemon-stop atomspace)
(daemon-stop llama)
(daemon-stop epu)
(daemon-stop clp)
(daemon-stop esrp)

(format #t "  ✓ All daemons stopped cleanly~%")

(format #t "~%")
(format #t "╔════════════════════════════════════════════════════════════╗~%")
(format #t "║  Full NNECCO-A9NN System Integration Complete             ║~%")
(format #t "║                                                            ║~%")
(format #t "║  🌳 Knowledge grows in AtomSpace hypergraphs               ║~%")
(format #t "║  🧠 Experiences persist in episodic memory                 ║~%")
(format #t "║  💾 Learning stored in replay buffer                       ║~%")
(format #t "║  💜 Personality shapes cognitive frames                    ║~%")
(format #t "║  🌊 All integrated through pure daemon architecture        ║~%")
(format #t "╚════════════════════════════════════════════════════════════╝~%")
(format #t "~%")
