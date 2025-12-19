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

(define-module (gnu opencog nnecco)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (gnu opencog daemons)
  #:export (;; Echo State Reservoir Processor (ESRP)
            make-esrp-daemon
            esrp-forward
            esrp-adapt-parameters
            esrp-reset
            
            ;; Consciousness Layer Processor (CLP)
            make-clp-daemon
            clp-process-frame
            clp-get-layer
            
            ;; Emotion Processing Unit (EPU)
            make-epu-daemon
            epu-set-emotion
            epu-get-emotion
            epu-modulate-reservoir
            
            ;; LLaMA Orchestrator
            make-llama-orchestrator-daemon
            llama-generate
            llama-get-status
            
            ;; NNECCO Agent
            make-nnecco-agent-daemon
            nnecco-process
            nnecco-echobeat
            nnecco-get-hardware-status))

;;;
;;; Echo State Reservoir Processor (ESRP) Daemon
;;;
;;; Implements reservoir computing with spectral radius control
;;; and emotional modulation
;;;

(define* (make-esrp-daemon #:key
                          (name "esrp")
                          (reservoir-size 847)
                          (input-dim 768)
                          (output-dim 256)
                          (spectral-radius 0.9)
                          (leak-rate 0.3)
                          (input-scaling 1.0))
  "Create Echo State Reservoir Processor daemon."
  (let ((daemon (make-daemon name 'esrp #:auto-register #t))
        (state (make-hash-table))
        (config (make-hash-table)))
    
    ;; Store configuration
    (hash-set! config 'reservoir-size reservoir-size)
    (hash-set! config 'input-dim input-dim)
    (hash-set! config 'output-dim output-dim)
    (hash-set! config 'spectral-radius spectral-radius)
    (hash-set! config 'leak-rate leak-rate)
    (hash-set! config 'input-scaling input-scaling)
    
    ;; Initialize reservoir state (simplified as vectors)
    (hash-set! state 'reservoir-state (make-vector reservoir-size 0))
    (hash-set! state 'output-state (make-vector output-dim 0))
    (hash-set! state 'config config)
    
    ;; Store state in daemon metrics
    (daemon-set-metric! daemon 'esrp-state state)
    
    daemon))

(define (esrp-daemon-loop daemon)
  "Main loop for ESRP daemon - processes reservoir updates."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Check for messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('forward
             (let* ((payload (message-payload msg))
                    (input (assoc-ref payload 'input))
                    (result (esrp-forward daemon input)))
               ;; Send result back
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'esrp-result
                                        `((output . ,result))))))))
            
            ('adapt
             (let ((payload (message-payload msg)))
               (esrp-adapt-parameters daemon
                                     (assoc-ref payload 'arousal)
                                     (assoc-ref payload 'frame))))
            
            ('reset
             (esrp-reset daemon))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      ;; Update metrics
      (daemon-set-metric! daemon 'cycles
                         (+ 1 (or (daemon-get-metric daemon 'cycles) 0)))
      
      (usleep 10000)
      (loop))))

(define (esrp-forward daemon input)
  "Forward pass through reservoir."
  (let* ((state (daemon-get-metric daemon 'esrp-state))
         (config (hash-ref state 'config))
         (reservoir-state (hash-ref state 'reservoir-state))
         (output-state (hash-ref state 'output-state))
         (leak-rate (hash-ref config 'leak-rate))
         (input-scaling (hash-ref config 'input-scaling)))
    
    ;; Simplified reservoir update (tanh activation)
    ;; In production would use proper matrix multiplication
    (let ((new-state (vector-map
                      (lambda (x i)
                        (let ((pre-activation (+ (* (- 1 leak-rate) x)
                                               (* leak-rate input-scaling
                                                  (if (< i (vector-length input))
                                                      (vector-ref input i)
                                                      0)))))
                          (tanh pre-activation)))
                      reservoir-state
                      (list->vector (iota (vector-length reservoir-state))))))
      
      ;; Update state
      (hash-set! state 'reservoir-state new-state)
      
      ;; Compute output (simplified projection)
      (let ((output (make-vector (hash-ref config 'output-dim) 0)))
        (hash-set! state 'output-state output)
        output))))

(define (esrp-adapt-parameters daemon arousal frame)
  "Adapt reservoir parameters based on emotional arousal and cognitive frame."
  (let* ((state (daemon-get-metric daemon 'esrp-state))
         (config (hash-ref state 'config))
         (frame-radii `((chaos . 0.95)
                       (strategy . 0.85)
                       (play . 0.90)
                       (social . 0.80))))
    
    ;; Update spectral radius based on frame
    (hash-set! config 'spectral-radius
              (or (assoc-ref frame-radii frame) 0.9))
    
    ;; Modulate input scaling by arousal
    (hash-set! config 'input-scaling
              (+ 1.0 (* 0.3 (or arousal 0.5))))
    
    (format #t "[~a] Adapted: spectral-radius=~a, input-scaling=~a~%"
            (daemon-name daemon)
            (hash-ref config 'spectral-radius)
            (hash-ref config 'input-scaling))))

(define (esrp-reset daemon)
  "Reset reservoir state."
  (let* ((state (daemon-get-metric daemon 'esrp-state))
         (config (hash-ref state 'config))
         (reservoir-size (hash-ref config 'reservoir-size)))
    (hash-set! state 'reservoir-state (make-vector reservoir-size 0))
    (format #t "[~a] Reservoir reset~%" (daemon-name daemon))))

;;;
;;; Consciousness Layer Processor (CLP) Daemon
;;;
;;; Multi-layer consciousness with frame-aware transitions
;;;

(define-record-type <consciousness-layer>
  (make-consciousness-layer level type)
  consciousness-layer?
  (level layer-level)
  (type layer-type))

(define *consciousness-layers*
  `((L0-Basic . ,(make-consciousness-layer 0 'reflexive))
    (L1-Experiential . ,(make-consciousness-layer 1 'frame-aware))
    (L2-Reflective . ,(make-consciousness-layer 2 'metacognitive))
    (L3-Meta . ,(make-consciousness-layer 3 'self-model))))

(define* (make-clp-daemon #:key (name "clp"))
  "Create Consciousness Layer Processor daemon."
  (let ((daemon (make-daemon name 'clp #:auto-register #t)))
    
    ;; Initialize with L1 experiential consciousness
    (daemon-set-metric! daemon 'current-layer
                       (assoc-ref *consciousness-layers* 'L1-Experiential))
    (daemon-set-metric! daemon 'current-frame 'neutral)
    (daemon-set-metric! daemon 'transition-history '())
    (daemon-set-metric! daemon 'message-queue '())
    
    daemon))

(define (clp-daemon-loop daemon)
  "Main loop for CLP daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('process-frame
             (let* ((payload (message-payload msg))
                    (frame (assoc-ref payload 'frame))
                    (input (assoc-ref payload 'input))
                    (reservoir-state (assoc-ref payload 'reservoir-state))
                    (result (clp-process-frame daemon frame input reservoir-state)))
               
               ;; Send result back if requested
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'clp-result
                                        `((result . ,result))))))))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (clp-process-frame daemon frame input reservoir-state)
  "Process input with frame-aware consciousness layer selection."
  (let* ((target-layer (clp-select-layer frame))
         (current-layer (daemon-get-metric daemon 'current-layer)))
    
    ;; Transition if needed
    (unless (eq? (layer-level target-layer) (layer-level current-layer))
      (clp-transition-to daemon target-layer
                        `((reason . frame-shift)
                          (from-frame . ,(daemon-get-metric daemon 'current-frame))
                          (to-frame . ,frame))))
    
    (daemon-set-metric! daemon 'current-layer target-layer)
    (daemon-set-metric! daemon 'current-frame frame)
    
    ;; Process at current layer
    (clp-process-at-layer daemon input reservoir-state)))

(define (clp-select-layer frame)
  "Select consciousness layer based on cognitive frame."
  (match frame
    ('chaos (assoc-ref *consciousness-layers* 'L1-Experiential))
    ('strategy (assoc-ref *consciousness-layers* 'L2-Reflective))
    ('play (assoc-ref *consciousness-layers* 'L1-Experiential))
    ('learning (assoc-ref *consciousness-layers* 'L3-Meta))
    (_ (assoc-ref *consciousness-layers* 'L1-Experiential))))

(define (clp-transition-to daemon layer metadata)
  "Transition to a new consciousness layer."
  (let* ((current-layer (daemon-get-metric daemon 'current-layer))
         (history (daemon-get-metric daemon 'transition-history))
         (transition `((from . ,(layer-level current-layer))
                      (to . ,(layer-level layer))
                      (timestamp . ,(current-time time-monotonic))
                      (metadata . ,metadata))))
    
    ;; Record transition
    (daemon-set-metric! daemon 'transition-history
                       (cons transition history))
    
    (format #t "[~a] Consciousness shift: L~a -> L~a~%"
            (daemon-name daemon)
            (layer-level current-layer)
            (layer-level layer))))

(define (clp-process-at-layer daemon input reservoir-state)
  "Process input at current consciousness layer."
  (let* ((layer (daemon-get-metric daemon 'current-layer))
         (frame (daemon-get-metric daemon 'current-frame)))
    
    (match (layer-level layer)
      (0 `((action . reflex) (response . ,input)))
      (1 `((action . perceive) (response . ,reservoir-state) (frame . ,frame)))
      (2 `((action . reflect) (response . ,reservoir-state) (quality . analyzed)))
      (3 `((action . introspect) (response . ,reservoir-state) (quality . meta)))
      (_ `((action . unknown))))))

(define (clp-get-layer daemon)
  "Get current consciousness layer."
  (daemon-get-metric daemon 'current-layer))

;;;
;;; Emotion Processing Unit (EPU) Daemon
;;;
;;; Discrete emotion channels with dimensional affect
;;;

(define *emotion-types*
  '((neutral . 1) (happy . 2) (excited . 3) (annoyed . 4)
    (thoughtful . 5) (confused . 6) (curious . 7)
    (determined . 8) (playful . 9) (sarcastic . 10)))

(define* (make-epu-daemon #:key (name "epu"))
  "Create Emotion Processing Unit daemon."
  (let ((daemon (make-daemon name 'epu #:auto-register #t)))
    
    ;; Initialize emotion state
    (daemon-set-metric! daemon 'emotion-vector (make-vector 10 0))
    (vector-set! (daemon-get-metric daemon 'emotion-vector) 0 1.0) ; neutral
    (daemon-set-metric! daemon 'current-emotion 'neutral)
    (daemon-set-metric! daemon 'valence 0.0)
    (daemon-set-metric! daemon 'arousal 0.5)
    (daemon-set-metric! daemon 'emotion-history '())
    
    daemon))

(define (epu-daemon-loop daemon)
  "Main loop for EPU daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('set-emotion
             (let ((payload (message-payload msg)))
               (epu-set-emotion daemon
                               (assoc-ref payload 'emotion)
                               (assoc-ref payload 'intensity)
                               (assoc-ref payload 'valence))))
            
            ('get-emotion
             (let ((payload (message-payload msg)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'emotion-state
                                        `((emotion . ,(epu-get-emotion daemon)))))))))
            
            ('modulate
             (let* ((payload (message-payload msg))
                    (frame (assoc-ref payload 'frame))
                    (modulation (epu-modulate-reservoir daemon frame)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'modulation-params
                                        `((params . ,modulation))))))))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (epu-set-emotion daemon emotion-type intensity valence)
  "Set current emotion state."
  (let* ((intensity (max 0 (min 1 (or intensity 0.5))))
         (valence (max -1 (min 1 (or valence 0))))
         (emotion-vector (make-vector 10 0))
         (emotion-idx (assoc-ref *emotion-types* emotion-type)))
    
    (when emotion-idx
      (vector-set! emotion-vector (- emotion-idx 1) intensity))
    
    (daemon-set-metric! daemon 'emotion-vector emotion-vector)
    (daemon-set-metric! daemon 'current-emotion emotion-type)
    (daemon-set-metric! daemon 'valence valence)
    (daemon-set-metric! daemon 'arousal intensity)
    
    ;; Record in history
    (let ((history (daemon-get-metric daemon 'emotion-history))
          (entry `((emotion . ,emotion-type)
                  (intensity . ,intensity)
                  (valence . ,valence)
                  (timestamp . ,(current-time time-monotonic)))))
      (daemon-set-metric! daemon 'emotion-history
                         (cons entry (take history (min 99 (length history))))))
    
    (format #t "[~a] Emotion set: ~a (intensity=~a, valence=~a)~%"
            (daemon-name daemon) emotion-type intensity valence)))

(define (epu-get-emotion daemon)
  "Get current emotion state."
  `((type . ,(daemon-get-metric daemon 'current-emotion))
    (intensity . ,(daemon-get-metric daemon 'arousal))
    (valence . ,(daemon-get-metric daemon 'valence))))

(define (epu-modulate-reservoir daemon frame)
  "Get reservoir modulation parameters based on emotional state and frame."
  (let* ((arousal (daemon-get-metric daemon 'arousal))
         (base-modulation `((input-scale-modifier . ,(+ 1.0 (* 0.3 arousal)))
                           (leak-rate-modifier . ,(- 1.0 (* 0.2 arousal)))
                           (exploration-bonus . ,(* arousal 0.2)))))
    
    ;; Frame-specific adjustments
    (let ((exploration (assoc-ref base-modulation 'exploration-bonus)))
      (match frame
        ('chaos (assoc-set! base-modulation 'exploration-bonus (* exploration 1.5)))
        ('strategy (assoc-set! base-modulation 'exploration-bonus (* exploration 0.5)))
        (_ base-modulation)))))

;;;
;;; LLaMA Orchestrator Daemon
;;;
;;; Manages 1-9 parallel LLaMA.cpp inference instances
;;;

(define* (make-llama-orchestrator-daemon #:key
                                        (name "llama-orchestrator")
                                        (num-instances 4)
                                        (base-port 8080))
  "Create LLaMA orchestrator daemon for parallel inference."
  (let ((daemon (make-daemon name 'llama-orchestrator #:auto-register #t)))
    
    ;; Initialize configuration
    (daemon-set-metric! daemon 'num-instances num-instances)
    (daemon-set-metric! daemon 'base-port base-port)
    (daemon-set-metric! daemon 'instances (make-vector num-instances #f))
    (daemon-set-metric! daemon 'instance-load (make-vector num-instances 0))
    (daemon-set-metric! daemon 'task-queue '())
    (daemon-set-metric! daemon 'completed-tasks '())
    (daemon-set-metric! daemon 'stats
                       `((total-requests . 0)
                         (total-tokens . 0)
                         (avg-latency . 0)))
    
    ;; Initialize instances
    (llama-initialize-instances daemon)
    
    daemon))

(define (llama-initialize-instances daemon)
  "Initialize LLaMA.cpp instances."
  (let* ((num-instances (daemon-get-metric daemon 'num-instances))
         (base-port (daemon-get-metric daemon 'base-port))
         (instances (daemon-get-metric daemon 'instances)))
    
    (format #t "[~a] 🧠 Initializing ~a LLaMA.cpp instances...~%"
            (daemon-name daemon) num-instances)
    
    (do ((i 0 (+ i 1)))
        ((>= i num-instances))
      (let ((port (+ base-port i))
            (instance `((id . ,(+ i 1))
                       (port . ,(+ base-port i))
                       (url . ,(string-append "http://localhost:" 
                                             (number->string (+ base-port i))))
                       (active . #t)
                       (load . 0)
                       (tokens-processed . 0))))
        (vector-set! instances i instance)
        (format #t "  [~a] Starting on port ~a~%" (+ i 1) port)))
    
    (format #t "[~a] ✅ All instances ready~%" (daemon-name daemon))))

(define (llama-daemon-loop daemon)
  "Main loop for LLaMA orchestrator daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('generate
             (let* ((payload (message-payload msg))
                    (prompt (assoc-ref payload 'prompt))
                    (config (assoc-ref payload 'config))
                    (result (llama-generate daemon prompt config)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'llama-result
                                        `((result . ,result))))))))
            
            ('get-status
             (let ((payload (message-payload msg)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'llama-status
                                        `((status . ,(llama-get-status daemon)))))))))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (llama-generate daemon prompt config)
  "Generate text using least-loaded LLaMA instance."
  (let* ((instance (llama-select-instance daemon))
         (task `((id . ,(string-append "task_" (number->string (current-time time-monotonic))))
                (prompt . ,prompt)
                (config . ,config)
                (instance . ,(assoc-ref instance 'id))
                (timestamp . ,(current-time time-monotonic)))))
    
    (if instance
        (begin
          ;; Increment load
          (let ((instance-load (daemon-get-metric daemon 'instance-load))
                (instance-id (- (assoc-ref instance 'id) 1)))
            (vector-set! instance-load instance-id
                        (+ 1 (vector-ref instance-load instance-id))))
          
          ;; Simulate generation (placeholder)
          (let ((result `((task-id . ,(assoc-ref task 'id))
                         (instance-id . ,(assoc-ref instance 'id))
                         (response . "Generated text from LLaMA.cpp")
                         (tokens . 128)
                         (latency . 0.35))))
            
            ;; Decrement load
            (let ((instance-load (daemon-get-metric daemon 'instance-load))
                  (instance-id (- (assoc-ref instance 'id) 1)))
              (vector-set! instance-load instance-id
                          (- (vector-ref instance-load instance-id) 1)))
            
            ;; Update stats
            (let* ((stats (daemon-get-metric daemon 'stats))
                   (total-requests (+ 1 (assoc-ref stats 'total-requests)))
                   (total-tokens (+ 128 (assoc-ref stats 'total-tokens))))
              (daemon-set-metric! daemon 'stats
                                 `((total-requests . ,total-requests)
                                   (total-tokens . ,total-tokens)
                                   (avg-latency . 0.35))))
            
            result))
        `((error . "No available instances")))))

(define (llama-select-instance daemon)
  "Select least-loaded LLaMA instance."
  (let* ((instances (daemon-get-metric daemon 'instances))
         (instance-load (daemon-get-metric daemon 'instance-load))
         (num-instances (daemon-get-metric daemon 'num-instances)))
    
    (let loop ((i 0)
               (min-load 999999)
               (selected #f))
      (if (>= i num-instances)
          selected
          (let ((instance (vector-ref instances i))
                (load (vector-ref instance-load i)))
            (if (and (assoc-ref instance 'active)
                    (< load min-load))
                (loop (+ i 1) load instance)
                (loop (+ i 1) min-load selected)))))))

(define (llama-get-status daemon)
  "Get LLaMA orchestrator status."
  (let* ((instances (daemon-get-metric daemon 'instances))
         (instance-load (daemon-get-metric daemon 'instance-load))
         (num-instances (daemon-get-metric daemon 'num-instances))
         (stats (daemon-get-metric daemon 'stats))
         (instance-status '()))
    
    (do ((i 0 (+ i 1)))
        ((>= i num-instances))
      (let ((instance (vector-ref instances i)))
        (set! instance-status
              (cons `((id . ,(assoc-ref instance 'id))
                     (port . ,(assoc-ref instance 'port))
                     (active . ,(assoc-ref instance 'active))
                     (current-load . ,(vector-ref instance-load i))
                     (tokens-processed . ,(assoc-ref instance 'tokens-processed)))
                    instance-status))))
    
    `((instances . ,(reverse instance-status))
      (queue-length . ,(length (daemon-get-metric daemon 'task-queue)))
      (completed-count . ,(length (daemon-get-metric daemon 'completed-tasks)))
      (stats . ,stats))))

;;;
;;; NNECCO Agent Daemon
;;;
;;; Main orchestrator that coordinates all NNECCO components
;;;

(define *echobeats-stages*
  '("PERCEIVE" "ATTEND" "REPRESENT" "REASON"
    "EMOTE" "INTEND" "ACT" "REFLECT"
    "LEARN" "CONSOLIDATE" "PRUNE" "REST"))

(define* (make-nnecco-agent-daemon #:key
                                   (name "nnecco-agent")
                                   (esrp-daemon #f)
                                   (clp-daemon #f)
                                   (epu-daemon #f)
                                   (llama-daemon #f))
  "Create NNECCO Agent daemon that orchestrates all components."
  (let ((daemon (make-daemon name 'nnecco-agent #:auto-register #t)))
    
    ;; Store component daemon references
    (daemon-set-metric! daemon 'esrp-daemon esrp-daemon)
    (daemon-set-metric! daemon 'clp-daemon clp-daemon)
    (daemon-set-metric! daemon 'epu-daemon epu-daemon)
    (daemon-set-metric! daemon 'llama-daemon llama-daemon)
    
    ;; Initialize EchoBeats state
    (daemon-set-metric! daemon 'echobeats-phase 1)
    (daemon-set-metric! daemon 'current-frame 'neutral)
    (daemon-set-metric! daemon 'last-input #f)
    (daemon-set-metric! daemon 'last-output #f)
    
    ;; Hardware registers (virtual)
    (daemon-set-metric! daemon 'registers
                       `((ESRP_STATUS . 0)
                         (CLP_LAYER . 1)
                         (EPU_STATE . 0)
                         (LLAMA_LOAD . 0)
                         (CYCLE_COUNT . 0)))
    
    daemon))

(define (nnecco-daemon-loop daemon)
  "Main loop for NNECCO agent daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('process
             (let* ((payload (message-payload msg))
                    (input (assoc-ref payload 'input))
                    (context (assoc-ref payload 'context))
                    (result (nnecco-process daemon input context)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'nnecco-result
                                        `((result . ,result))))))))
            
            ('echobeat
             (nnecco-echobeat daemon))
            
            ('get-status
             (let ((payload (message-payload msg)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'nnecco-status
                                        `((status . ,(nnecco-get-hardware-status daemon)))))))))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (nnecco-process daemon input context)
  "Process input through NNECCO cognitive pipeline."
  (daemon-set-metric! daemon 'last-input input)
  
  (let* ((esrp (daemon-get-metric daemon 'esrp-daemon))
         (clp (daemon-get-metric daemon 'clp-daemon))
         (epu (daemon-get-metric daemon 'epu-daemon))
         (llama (daemon-get-metric daemon 'llama-daemon)))
    
    ;; Simple processing result
    `((status . success)
      (input . ,input)
      (context . ,context)
      (timestamp . ,(current-time time-monotonic)))))

(define (nnecco-echobeat daemon)
  "Execute one EchoBeats cognitive cycle phase."
  (let* ((phase (daemon-get-metric daemon 'echobeats-phase))
         (stage (list-ref *echobeats-stages* (- phase 1))))
    
    (format #t "[~a] 🌊 EchoBeats Phase ~a/12: ~a~%"
            (daemon-name daemon) phase stage)
    
    ;; Execute phase-specific logic
    (match stage
      ("PERCEIVE" (nnecco-perceive-phase daemon))
      ("REPRESENT" (nnecco-represent-phase daemon))
      ("REASON" (nnecco-reason-phase daemon))
      ("EMOTE" (nnecco-emote-phase daemon))
      ("REFLECT" (nnecco-reflect-phase daemon))
      ("CONSOLIDATE" (nnecco-consolidate-phase daemon))
      (_ #t))
    
    ;; Advance phase
    (let ((next-phase (if (>= phase 12) 1 (+ phase 1))))
      (daemon-set-metric! daemon 'echobeats-phase next-phase))
    
    ;; Update cycle count
    (let ((registers (daemon-get-metric daemon 'registers)))
      (daemon-set-metric! daemon 'registers
                         (assoc-set! registers 'CYCLE_COUNT
                                   (+ 1 (assoc-ref registers 'CYCLE_COUNT)))))))

(define (nnecco-perceive-phase daemon)
  "EchoBeats PERCEIVE phase."
  (daemon-set-metric! daemon 'current-frame 'neutral))

(define (nnecco-represent-phase daemon)
  "EchoBeats REPRESENT phase - update reservoir."
  (let ((esrp (daemon-get-metric daemon 'esrp-daemon)))
    (when esrp
      (daemon-send-message daemon esrp 'forward
                          `((input . #(0.5 0.5 0.5)))))))

(define (nnecco-reason-phase daemon)
  "EchoBeats REASON phase - LLaMA inference."
  (let ((llama (daemon-get-metric daemon 'llama-daemon)))
    (when llama
      (daemon-send-message daemon llama 'generate
                          `((prompt . "Cognitive reasoning..."))))))

(define (nnecco-emote-phase daemon)
  "EchoBeats EMOTE phase - update emotion."
  (let ((epu (daemon-get-metric daemon 'epu-daemon)))
    (when epu
      (daemon-send-message daemon epu 'set-emotion
                          `((emotion . curious)
                            (intensity . 0.7)
                            (valence . 0.5))))))

(define (nnecco-reflect-phase daemon)
  "EchoBeats REFLECT phase - consciousness processing."
  (let ((clp (daemon-get-metric daemon 'clp-daemon)))
    (when clp
      (daemon-send-message daemon clp 'process-frame
                          `((frame . ,(daemon-get-metric daemon 'current-frame))
                            (input . #f)
                            (reservoir-state . #f))))))

(define (nnecco-consolidate-phase daemon)
  "EchoBeats CONSOLIDATE phase - store in memory."
  (format #t "[~a] Consolidating memories...~%" (daemon-name daemon)))

(define (nnecco-get-hardware-status daemon)
  "Get NNECCO hardware status report."
  (let* ((registers (daemon-get-metric daemon 'registers))
         (phase (daemon-get-metric daemon 'echobeats-phase))
         (stage (list-ref *echobeats-stages* (- phase 1))))
    
    `((registers . ,registers)
      (echobeats-phase . ,phase)
      (echobeats-stage . ,stage)
      (current-frame . ,(daemon-get-metric daemon 'current-frame)))))
