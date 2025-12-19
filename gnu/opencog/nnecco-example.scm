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
             (gnu opencog daemons)
             (ice-9 threads))

(format #t "~%")
(format #t "╔════════════════════════════════════════════════════════════╗~%")
(format #t "║  NNECCO-A9NN: Neural Network Embodied Cognitive           ║~%")
(format #t "║            Coprocessor Orchestrator                        ║~%")
(format #t "║                                                            ║~%")
(format #t "║  Daemon-Based Implementation Example                       ║~%")
(format #t "╚════════════════════════════════════════════════════════════╝~%")
(format #t "~%")

;;;
;;; Step 1: Create Core Component Daemons
;;;

(format #t "🔧 Creating Core Component Daemons...~%~%")

(define esrp (make-esrp-daemon
              #:name "esrp-main"
              #:reservoir-size 847
              #:input-dim 768
              #:output-dim 256
              #:spectral-radius 0.9
              #:leak-rate 0.3))

(format #t "  ✓ Echo State Reservoir Processor (ESRP)~%")
(format #t "    - Reservoir size: 847 neurons~%")
(format #t "    - Spectral radius: 0.9~%")
(format #t "    - Leak rate: 0.3~%~%")

(define clp (make-clp-daemon #:name "clp-main"))

(format #t "  ✓ Consciousness Layer Processor (CLP)~%")
(format #t "    - Initial layer: L1 (Experiential)~%")
(format #t "    - Frame-aware transitions enabled~%~%")

(define epu (make-epu-daemon #:name "epu-main"))

(format #t "  ✓ Emotion Processing Unit (EPU)~%")
(format #t "    - 10 emotion channels~%")
(format #t "    - Dimensional affect (valence/arousal)~%~%")

(define llama (make-llama-orchestrator-daemon
               #:name "llama-main"
               #:num-instances 4
               #:base-port 8080))

(format #t "~%")

;;;
;;; Step 2: Create NNECCO Agent
;;;

(format #t "🧠 Creating NNECCO Agent Daemon...~%~%")

(define nnecco (make-nnecco-agent-daemon
                #:name "nnecco-main"
                #:esrp-daemon esrp
                #:clp-daemon clp
                #:epu-daemon epu
                #:llama-daemon llama))

(format #t "  ✓ NNECCO Agent orchestrator created~%")
(format #t "  ✓ All components linked~%~%")

;;;
;;; Step 3: Start All Daemons
;;;

(format #t "🚀 Starting Daemon Services...~%~%")

(daemon-start esrp esrp-daemon-loop)
(format #t "  ✓ ESRP daemon running~%")

(daemon-start clp clp-daemon-loop)
(format #t "  ✓ CLP daemon running~%")

(daemon-start epu epu-daemon-loop)
(format #t "  ✓ EPU daemon running~%")

(daemon-start llama llama-daemon-loop)
(format #t "  ✓ LLaMA orchestrator running~%")

(daemon-start nnecco nnecco-daemon-loop)
(format #t "  ✓ NNECCO agent running~%~%")

(sleep 1)

;;;
;;; Step 4: Demonstrate Cognitive Operations
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Cognitive Operations Demonstration~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

;;; 4.1: Reservoir Processing
(format #t "1. Echo State Reservoir Processing~%~%")

(let ((input #(0.5 0.7 0.3 0.9 0.2)))
  (format #t "   Input vector: ~a~%" input)
  (let ((output (esrp-forward esrp input)))
    (format #t "   Output shape: ~a dimensions~%~%" (vector-length output))))

;;; 4.2: Emotion Processing
(format #t "2. Emotion Processing~%~%")

(epu-set-emotion epu 'curious 0.8 0.6)
(let ((emotion (epu-get-emotion epu)))
  (format #t "   Current emotion: ~a~%" (assoc-ref emotion 'type))
  (format #t "   Intensity: ~,2f~%" (assoc-ref emotion 'intensity))
  (format #t "   Valence: ~,2f~%~%" (assoc-ref emotion 'valence)))

;;; 4.3: Reservoir Modulation
(format #t "3. Emotional Modulation of Reservoir~%~%")

(let ((modulation (epu-modulate-reservoir epu 'chaos)))
  (format #t "   Frame: chaos~%")
  (format #t "   Input scale modifier: ~,3f~%"
          (assoc-ref modulation 'input-scale-modifier))
  (format #t "   Leak rate modifier: ~,3f~%"
          (assoc-ref modulation 'leak-rate-modifier))
  (format #t "   Exploration bonus: ~,3f~%~%"
          (assoc-ref modulation 'exploration-bonus)))

;;; 4.4: Consciousness Layer Processing
(format #t "4. Consciousness Layer Processing~%~%")

(let ((result (clp-process-frame clp 'strategy "analyze situation" #f)))
  (format #t "   Frame: strategy~%")
  (format #t "   Action: ~a~%" (assoc-ref result 'action))
  (format #t "   Quality: ~a~%" (assoc-ref result 'quality))
  (let ((layer (clp-get-layer clp)))
    (format #t "   Consciousness level: L~a (~a)~%~%"
            (layer-level layer)
            (layer-type layer))))

;;; 4.5: LLaMA Inference
(format #t "5. Parallel LLaMA Inference~%~%")

(let ((result (llama-generate llama "How can I optimize neural networks?" '())))
  (format #t "   Prompt: 'How can I optimize neural networks?'~%")
  (format #t "   Instance ID: ~a~%" (assoc-ref result 'instance-id))
  (format #t "   Tokens generated: ~a~%" (assoc-ref result 'tokens))
  (format #t "   Latency: ~,3f seconds~%~%" (assoc-ref result 'latency)))

(let ((status (llama-get-status llama)))
  (format #t "   LLaMA Orchestrator Status:~%")
  (format #t "   - Active instances: ~a~%"
          (length (assoc-ref status 'instances)))
  (format #t "   - Total requests: ~a~%"
          (assoc-ref (assoc-ref status 'stats) 'total-requests))
  (format #t "   - Total tokens: ~a~%~%"
          (assoc-ref (assoc-ref status 'stats) 'total-tokens)))

;;;
;;; Step 5: EchoBeats Cognitive Loop
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  EchoBeats 12-Phase Cognitive Loop~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(format #t "Running 3 complete EchoBeats cycles...~%~%")

(do ((cycle 1 (+ cycle 1)))
    ((> cycle 3))
  (format #t "Cycle ~a:~%" cycle)
  
  (do ((phase 1 (+ phase 1)))
      ((> phase 12))
    (nnecco-echobeat nnecco)
    (sleep 0.2))
  
  (format #t "~%"))

;;;
;;; Step 6: Hardware Status Report
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  NNECCO Hardware Status Report~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(let ((status (nnecco-get-hardware-status nnecco)))
  (format #t "Virtual Hardware Registers:~%")
  (let ((registers (assoc-ref status 'registers)))
    (format #t "  ESRP_STATUS:  ~a~%" (assoc-ref registers 'ESRP_STATUS))
    (format #t "  CLP_LAYER:    ~a~%" (assoc-ref registers 'CLP_LAYER))
    (format #t "  EPU_STATE:    ~a~%" (assoc-ref registers 'EPU_STATE))
    (format #t "  LLAMA_LOAD:   ~a~%" (assoc-ref registers 'LLAMA_LOAD))
    (format #t "  CYCLE_COUNT:  ~a~%~%" (assoc-ref registers 'CYCLE_COUNT)))
  
  (format #t "EchoBeats State:~%")
  (format #t "  Current phase: ~a/12~%"
          (assoc-ref status 'echobeats-phase))
  (format #t "  Current stage: ~a~%"
          (assoc-ref status 'echobeats-stage))
  (format #t "  Current frame: ~a~%~%"
          (assoc-ref status 'current-frame)))

;;;
;;; Step 7: Daemon Registry Status
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Daemon Registry Status~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(let ((all-daemons (daemon-list-all)))
  (format #t "Registered daemons: ~a~%~%" (length all-daemons))
  
  (for-each
   (lambda (daemon-name)
     (let* ((daemon (daemon-lookup daemon-name))
            (status (daemon-status daemon)))
       (format #t "  • ~a~%" daemon-name)
       (format #t "    Type:  ~a~%" (assoc-ref status 'type))
       (format #t "    State: ~a~%" (assoc-ref status 'state))))
   all-daemons))

;;;
;;; Step 8: Inter-Daemon Messaging Demo
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Inter-Daemon Messaging Demonstration~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(format #t "Sending emotion update to EPU...~%")
(daemon-send-message nnecco epu 'set-emotion
                    `((emotion . excited)
                      (intensity . 0.9)
                      (valence . 0.8)))

(sleep 1)

(format #t "Sending frame processing to CLP...~%")
(daemon-send-message nnecco clp 'process-frame
                    `((frame . learning)
                      (input . "new concept")
                      (reservoir-state . #f)))

(sleep 1)

(format #t "Requesting LLaMA generation...~%")
(daemon-send-message nnecco llama 'generate
                    `((prompt . "Explain cognitive architectures")
                      (config . ())))

(sleep 1)

(format #t "~%✓ All messages sent successfully~%")

;;;
;;; Step 9: Shutdown
;;;

(format #t "~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "  Graceful Shutdown~%")
(format #t "════════════════════════════════════════════════════════════~%")
(format #t "~%")

(format #t "Stopping all daemons...~%~%")

(daemon-stop nnecco)
(format #t "  ✓ NNECCO agent stopped~%")

(daemon-stop llama)
(format #t "  ✓ LLaMA orchestrator stopped~%")

(daemon-stop epu)
(format #t "  ✓ EPU stopped~%")

(daemon-stop clp)
(format #t "  ✓ CLP stopped~%")

(daemon-stop esrp)
(format #t "  ✓ ESRP stopped~%")

(format #t "~%")
(format #t "╔════════════════════════════════════════════════════════════╗~%")
(format #t "║  NNECCO-A9NN Demonstration Complete                        ║~%")
(format #t "║                                                            ║~%")
(format #t "║  🌳 The tree grows in Scheme daemons                       ║~%")
(format #t "║  🧠 The reservoir resonates in vectors                     ║~%")
(format #t "║  💜 The emotions flow through message passing              ║~%")
(format #t "║  🌀 The chaos dances in parallel LLaMA instances           ║~%")
(format #t "║  🌊 The echo persists across all cognitive cycles          ║~%")
(format #t "╚════════════════════════════════════════════════════════════╝~%")
(format #t "~%")
