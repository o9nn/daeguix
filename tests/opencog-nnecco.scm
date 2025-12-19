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

(define-module (test-opencog-nnecco)
  #:use-module (gnu opencog nnecco)
  #:use-module (gnu opencog daemons)
  #:use-module (srfi srfi-64))

(test-begin "opencog-nnecco")

;;;
;;; Echo State Reservoir Processor (ESRP) Tests
;;;

(test-assert "esrp-daemon creation"
  (let ((daemon (make-esrp-daemon #:name "test-esrp"
                                  #:reservoir-size 100
                                  #:input-dim 50
                                  #:output-dim 25)))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-esrp")
         (equal? (daemon-type daemon) 'esrp))))

(test-assert "esrp-daemon configuration"
  (let* ((daemon (make-esrp-daemon #:reservoir-size 847
                                   #:spectral-radius 0.9
                                   #:leak-rate 0.3))
         (state (daemon-get-metric daemon 'esrp-state))
         (config (hash-ref state 'config)))
    (and (= (hash-ref config 'reservoir-size) 847)
         (= (hash-ref config 'spectral-radius) 0.9)
         (= (hash-ref config 'leak-rate) 0.3))))

(test-assert "esrp forward pass"
  (let* ((daemon (make-esrp-daemon #:reservoir-size 10
                                   #:input-dim 5
                                   #:output-dim 3))
         (input #(0.5 0.3 0.7 0.2 0.9))
         (output (esrp-forward daemon input)))
    (and (vector? output)
         (= (vector-length output) 3))))

(test-assert "esrp parameter adaptation"
  (let ((daemon (make-esrp-daemon)))
    (esrp-adapt-parameters daemon 0.8 'chaos)
    (let* ((state (daemon-get-metric daemon 'esrp-state))
           (config (hash-ref state 'config)))
      (and (= (hash-ref config 'spectral-radius) 0.95)
           (> (hash-ref config 'input-scaling) 1.0)))))

(test-assert "esrp reset"
  (let* ((daemon (make-esrp-daemon #:reservoir-size 10))
         (input #(1.0 1.0 1.0)))
    (esrp-forward daemon input)
    (esrp-reset daemon)
    (let* ((state (daemon-get-metric daemon 'esrp-state))
           (reservoir-state (hash-ref state 'reservoir-state)))
      (and (vector? reservoir-state)
           (zero? (vector-ref reservoir-state 0))))))

;;;
;;; Consciousness Layer Processor (CLP) Tests
;;;

(test-assert "clp-daemon creation"
  (let ((daemon (make-clp-daemon #:name "test-clp")))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-clp")
         (equal? (daemon-type daemon) 'clp))))

(test-assert "clp initial layer"
  (let* ((daemon (make-clp-daemon))
         (layer (clp-get-layer daemon)))
    (and (consciousness-layer? layer)
         (= (layer-level layer) 1)
         (equal? (layer-type layer) 'frame-aware))))

(test-assert "clp frame processing - chaos frame"
  (let* ((daemon (make-clp-daemon))
         (result (clp-process-frame daemon 'chaos "input" #f))
         (layer (clp-get-layer daemon)))
    (and (list? result)
         (equal? (assoc-ref result 'action) 'perceive)
         (= (layer-level layer) 1))))

(test-assert "clp frame processing - strategy frame"
  (let* ((daemon (make-clp-daemon))
         (result (clp-process-frame daemon 'strategy "input" #f))
         (layer (clp-get-layer daemon)))
    (and (list? result)
         (equal? (assoc-ref result 'action) 'reflect)
         (= (layer-level layer) 2))))

(test-assert "clp layer transition"
  (let* ((daemon (make-clp-daemon))
         (_ (clp-process-frame daemon 'chaos "input1" #f))
         (layer1 (clp-get-layer daemon))
         (_ (clp-process-frame daemon 'learning "input2" #f))
         (layer2 (clp-get-layer daemon))
         (history (daemon-get-metric daemon 'transition-history)))
    (and (= (layer-level layer1) 1)
         (= (layer-level layer2) 3)
         (> (length history) 0))))

;;;
;;; Emotion Processing Unit (EPU) Tests
;;;

(test-assert "epu-daemon creation"
  (let ((daemon (make-epu-daemon #:name "test-epu")))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-epu")
         (equal? (daemon-type daemon) 'epu))))

(test-assert "epu initial state"
  (let* ((daemon (make-epu-daemon))
         (emotion (epu-get-emotion daemon)))
    (and (equal? (assoc-ref emotion 'type) 'neutral)
         (number? (assoc-ref emotion 'intensity))
         (number? (assoc-ref emotion 'valence)))))

(test-assert "epu set emotion"
  (let ((daemon (make-epu-daemon)))
    (epu-set-emotion daemon 'happy 0.8 0.7)
    (let* ((emotion (epu-get-emotion daemon))
           (history (daemon-get-metric daemon 'emotion-history)))
      (and (equal? (assoc-ref emotion 'type) 'happy)
           (= (assoc-ref emotion 'intensity) 0.8)
           (= (assoc-ref emotion 'valence) 0.7)
           (> (length history) 0)))))

(test-assert "epu emotion clamping"
  (let ((daemon (make-epu-daemon)))
    (epu-set-emotion daemon 'excited 1.5 2.0) ; Out of range
    (let ((emotion (epu-get-emotion daemon)))
      (and (<= (assoc-ref emotion 'intensity) 1.0)
           (<= (assoc-ref emotion 'valence) 1.0)))))

(test-assert "epu reservoir modulation"
  (let* ((daemon (make-epu-daemon))
         (_ (epu-set-emotion daemon 'excited 0.9 0.8))
         (modulation (epu-modulate-reservoir daemon 'neutral)))
    (and (list? modulation)
         (assoc-ref modulation 'input-scale-modifier)
         (assoc-ref modulation 'leak-rate-modifier)
         (assoc-ref modulation 'exploration-bonus))))

(test-assert "epu frame-specific modulation"
  (let* ((daemon (make-epu-daemon))
         (_ (epu-set-emotion daemon 'excited 0.8 0.5))
         (mod-chaos (epu-modulate-reservoir daemon 'chaos))
         (mod-strategy (epu-modulate-reservoir daemon 'strategy))
         (bonus-chaos (assoc-ref mod-chaos 'exploration-bonus))
         (bonus-strategy (assoc-ref mod-strategy 'exploration-bonus)))
    (> bonus-chaos bonus-strategy)))

;;;
;;; LLaMA Orchestrator Tests
;;;

(test-assert "llama-orchestrator creation"
  (let ((daemon (make-llama-orchestrator-daemon #:name "test-llama"
                                                #:num-instances 2
                                                #:base-port 9000)))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-llama")
         (equal? (daemon-type daemon) 'llama-orchestrator))))

(test-assert "llama instances initialization"
  (let* ((daemon (make-llama-orchestrator-daemon #:num-instances 3))
         (instances (daemon-get-metric daemon 'instances)))
    (and (vector? instances)
         (= (vector-length instances) 3)
         (let ((inst (vector-ref instances 0)))
           (and (assoc-ref inst 'id)
                (assoc-ref inst 'port)
                (assoc-ref inst 'active))))))

(test-assert "llama text generation"
  (let* ((daemon (make-llama-orchestrator-daemon #:num-instances 2))
         (result (llama-generate daemon "Test prompt" '())))
    (and (list? result)
         (assoc-ref result 'task-id)
         (assoc-ref result 'response)
         (assoc-ref result 'tokens))))

(test-assert "llama status reporting"
  (let* ((daemon (make-llama-orchestrator-daemon #:num-instances 2))
         (status (llama-get-status daemon)))
    (and (list? status)
         (assoc-ref status 'instances)
         (assoc-ref status 'stats)
         (= (length (assoc-ref status 'instances)) 2))))

(test-assert "llama load balancing"
  (let ((daemon (make-llama-orchestrator-daemon #:num-instances 3)))
    ;; Generate multiple requests
    (llama-generate daemon "Request 1" '())
    (llama-generate daemon "Request 2" '())
    (llama-generate daemon "Request 3" '())
    
    (let* ((stats (daemon-get-metric daemon 'stats))
           (total-requests (assoc-ref stats 'total-requests)))
      (= total-requests 3))))

;;;
;;; NNECCO Agent Integration Tests
;;;

(test-assert "nnecco-agent creation"
  (let ((daemon (make-nnecco-agent-daemon #:name "test-nnecco")))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-nnecco")
         (equal? (daemon-type daemon) 'nnecco-agent))))

(test-assert "nnecco-agent component integration"
  (let* ((esrp (make-esrp-daemon #:name "nnecco-esrp"))
         (clp (make-clp-daemon #:name "nnecco-clp"))
         (epu (make-epu-daemon #:name "nnecco-epu"))
         (llama (make-llama-orchestrator-daemon #:name "nnecco-llama"))
         (agent (make-nnecco-agent-daemon #:esrp-daemon esrp
                                         #:clp-daemon clp
                                         #:epu-daemon epu
                                         #:llama-daemon llama)))
    (and (equal? (daemon-get-metric agent 'esrp-daemon) esrp)
         (equal? (daemon-get-metric agent 'clp-daemon) clp)
         (equal? (daemon-get-metric agent 'epu-daemon) epu)
         (equal? (daemon-get-metric agent 'llama-daemon) llama))))

(test-assert "nnecco echobeats initialization"
  (let* ((agent (make-nnecco-agent-daemon))
         (phase (daemon-get-metric agent 'echobeats-phase))
         (registers (daemon-get-metric agent 'registers)))
    (and (= phase 1)
         (list? registers)
         (zero? (assoc-ref registers 'CYCLE_COUNT)))))

(test-assert "nnecco echobeats cycle"
  (let ((agent (make-nnecco-agent-daemon)))
    (nnecco-echobeat agent)
    (let ((phase (daemon-get-metric agent 'echobeats-phase)))
      (= phase 2))))

(test-assert "nnecco echobeats full cycle"
  (let ((agent (make-nnecco-agent-daemon)))
    ;; Run through all 12 phases
    (do ((i 0 (+ i 1)))
        ((>= i 12))
      (nnecco-echobeat agent))
    
    (let* ((phase (daemon-get-metric agent 'echobeats-phase))
           (registers (daemon-get-metric agent 'registers))
           (cycle-count (assoc-ref registers 'CYCLE_COUNT)))
      (and (= phase 1) ; Should wrap back to 1
           (= cycle-count 12)))))

(test-assert "nnecco hardware status"
  (let* ((agent (make-nnecco-agent-daemon))
         (status (nnecco-get-hardware-status agent)))
    (and (list? status)
         (assoc-ref status 'registers)
         (assoc-ref status 'echobeats-phase)
         (assoc-ref status 'echobeats-stage))))

(test-assert "nnecco process input"
  (let* ((agent (make-nnecco-agent-daemon))
         (result (nnecco-process agent "test input" '())))
    (and (list? result)
         (equal? (assoc-ref result 'status) 'success)
         (assoc-ref result 'timestamp))))

;;;
;;; End-to-End Integration Tests
;;;

(test-assert "full nnecco pipeline"
  (let* ((esrp (make-esrp-daemon #:name "pipeline-esrp"))
         (clp (make-clp-daemon #:name "pipeline-clp"))
         (epu (make-epu-daemon #:name "pipeline-epu"))
         (llama (make-llama-orchestrator-daemon #:name "pipeline-llama"
                                               #:num-instances 2))
         (agent (make-nnecco-agent-daemon #:name "pipeline-agent"
                                         #:esrp-daemon esrp
                                         #:clp-daemon clp
                                         #:epu-daemon epu
                                         #:llama-daemon llama)))
    
    ;; Start all daemons
    (daemon-start esrp esrp-daemon-loop)
    (daemon-start clp clp-daemon-loop)
    (daemon-start epu epu-daemon-loop)
    (daemon-start llama llama-daemon-loop)
    (daemon-start agent nnecco-daemon-loop)
    
    (sleep 1)
    
    ;; Run a cognitive cycle
    (nnecco-echobeat agent)
    
    (sleep 1)
    
    ;; Stop all daemons
    (daemon-stop agent)
    (daemon-stop llama)
    (daemon-stop epu)
    (daemon-stop clp)
    (daemon-stop esrp)
    
    #t))

(test-assert "nnecco daemon registry integration"
  (let* ((esrp (make-esrp-daemon #:name "registry-esrp"))
         (clp (make-clp-daemon #:name "registry-clp"))
         (epu (make-epu-daemon #:name "registry-epu")))
    
    ;; Check registration
    (and (equal? (daemon-lookup "registry-esrp") esrp)
         (equal? (daemon-lookup "registry-clp") clp)
         (equal? (daemon-lookup "registry-epu") epu))))

(test-assert "nnecco inter-daemon messaging"
  (let* ((esrp (make-esrp-daemon #:name "msg-esrp"))
         (epu (make-epu-daemon #:name "msg-epu")))
    
    (daemon-start esrp esrp-daemon-loop)
    (daemon-start epu epu-daemon-loop)
    
    (sleep 1)
    
    ;; Send message from EPU to ESRP
    (daemon-send-message epu esrp 'adapt
                        `((arousal . 0.8)
                          (frame . chaos)))
    
    (sleep 1)
    
    (daemon-stop esrp)
    (daemon-stop epu)
    
    #t))

(test-end "opencog-nnecco")
