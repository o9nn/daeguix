# NNECCO-A9NN: Neural Network Embodied Cognitive Coprocessor Orchestrator

## Overview

NNECCO-A9NN is a comprehensive daemon-based cognitive architecture that implements a Neural Network Embodied Cognitive Coprocessor Orchestrator entirely using pure Scheme daemons and message passing.

## Architecture

NNECCO-A9NN synthesizes multiple cognitive paradigms into a unified daemon-based system:

- **Echo State Networks**: Reservoir computing with spectral radius control
- **Consciousness Layers**: Multi-level consciousness with frame-aware transitions
- **Emotion Processing**: Discrete emotion channels with dimensional affect
- **Parallel LLaMA Orchestration**: 1-9 parallel inference instances for local AI
- **EchoBeats**: 12-phase cognitive loop for continuous processing

### Core Components

#### 1. Echo State Reservoir Processor (ESRP) Daemon

Implements reservoir computing with:
- Configurable reservoir size (default: 847 neurons)
- Spectral radius control for stability
- Leak rate for temporal dynamics
- Emotional modulation of parameters
- Frame-aware adaptation

**Message Protocol:**
```scheme
;; Forward pass
(daemon-send-message sender esrp 'forward
  `((input . #(0.5 0.3 0.7))
    (reply-to . "sender-name")))

;; Adapt parameters
(daemon-send-message sender esrp 'adapt
  `((arousal . 0.8)
    (frame . chaos)))

;; Reset state
(daemon-send-message sender esrp 'reset '())
```

#### 2. Consciousness Layer Processor (CLP) Daemon

Multi-layer consciousness system:
- **L0 Basic**: Reflexive processing
- **L1 Experiential**: Frame-aware perception
- **L2 Reflective**: Metacognitive analysis
- **L3 Meta**: Self-model reasoning

Frame-to-layer mapping:
- `chaos` → L1 Experiential
- `strategy` → L2 Reflective
- `play` → L1 Experiential
- `learning` → L3 Meta

**Message Protocol:**
```scheme
;; Process with frame awareness
(daemon-send-message sender clp 'process-frame
  `((frame . strategy)
    (input . "analyze this")
    (reservoir-state . #f)
    (reply-to . "sender-name")))
```

#### 3. Emotion Processing Unit (EPU) Daemon

Manages emotional state with:
- 10 discrete emotion channels
- Dimensional affect (valence, arousal)
- Emotion history tracking
- Reservoir modulation parameters

**Supported Emotions:**
neutral, happy, excited, annoyed, thoughtful, confused, curious, determined, playful, sarcastic

**Message Protocol:**
```scheme
;; Set emotion
(daemon-send-message sender epu 'set-emotion
  `((emotion . curious)
    (intensity . 0.7)
    (valence . 0.5)))

;; Get current emotion
(daemon-send-message sender epu 'get-emotion
  `((reply-to . "sender-name")))

;; Get reservoir modulation
(daemon-send-message sender epu 'modulate
  `((frame . chaos)
    (reply-to . "sender-name")))
```

#### 4. LLaMA Orchestrator Daemon

Manages parallel LLaMA.cpp instances:
- 1-9 configurable instances
- Load balancing across instances
- Port-based addressing (8080-8088)
- Task queueing and completion tracking
- Performance statistics

**Message Protocol:**
```scheme
;; Generate text
(daemon-send-message sender llama 'generate
  `((prompt . "Your prompt here")
    (config . ((temperature . 0.7)
               (max-tokens . 256)))
    (reply-to . "sender-name")))

;; Get status
(daemon-send-message sender llama 'get-status
  `((reply-to . "sender-name")))
```

#### 5. NNECCO Agent Daemon

Main orchestrator coordinating all components:
- EchoBeats 12-phase cognitive loop
- Hardware-style register interface
- Component lifecycle management
- Inter-daemon message routing

**Message Protocol:**
```scheme
;; Process input
(daemon-send-message sender nnecco 'process
  `((input . "user input")
    (context . ())
    (reply-to . "sender-name")))

;; Execute one EchoBeats phase
(daemon-send-message sender nnecco 'echobeat '())

;; Get hardware status
(daemon-send-message sender nnecco 'get-status
  `((reply-to . "sender-name")))
```

## EchoBeats 12-Phase Cognitive Loop

The EchoBeats loop implements a complete cognitive cycle:

1. **PERCEIVE**: Frame-aware perception
2. **ATTEND**: Attention allocation
3. **REPRESENT**: Reservoir state update
4. **REASON**: LLaMA inference
5. **EMOTE**: Emotional state update
6. **INTEND**: Intention formation
7. **ACT**: Action execution
8. **REFLECT**: Consciousness processing
9. **LEARN**: Pattern learning
10. **CONSOLIDATE**: Memory storage
11. **PRUNE**: Memory cleanup
12. **REST**: System recovery

Each phase executes sequentially, with the cycle repeating continuously.

## Usage

### Basic Setup

```scheme
(use-modules (gnu opencog nnecco)
             (gnu opencog daemons))

;; Create component daemons
(define esrp (make-esrp-daemon #:name "esrp"))
(define clp (make-clp-daemon #:name "clp"))
(define epu (make-epu-daemon #:name "epu"))
(define llama (make-llama-orchestrator-daemon 
               #:name "llama"
               #:num-instances 4))

;; Create NNECCO agent
(define nnecco (make-nnecco-agent-daemon
                #:name "nnecco"
                #:esrp-daemon esrp
                #:clp-daemon clp
                #:epu-daemon epu
                #:llama-daemon llama))

;; Start all daemons
(daemon-start esrp esrp-daemon-loop)
(daemon-start clp clp-daemon-loop)
(daemon-start epu epu-daemon-loop)
(daemon-start llama llama-daemon-loop)
(daemon-start nnecco nnecco-daemon-loop)
```

### Running EchoBeats Cycles

```scheme
;; Run a single phase
(nnecco-echobeat nnecco)

;; Run complete cycle (12 phases)
(do ((i 0 (+ i 1)))
    ((>= i 12))
  (nnecco-echobeat nnecco))

;; Check status
(let ((status (nnecco-get-hardware-status nnecco)))
  (format #t "Phase: ~a/12~%"
          (assoc-ref status 'echobeats-phase))
  (format #t "Stage: ~a~%"
          (assoc-ref status 'echobeats-stage))
  (format #t "Cycle count: ~a~%"
          (assoc-ref (assoc-ref status 'registers) 'CYCLE_COUNT)))
```

### Emotional State Management

```scheme
;; Set emotion
(epu-set-emotion epu 'excited 0.9 0.7)

;; Get current emotion
(let ((emotion (epu-get-emotion epu)))
  (format #t "Emotion: ~a~%" (assoc-ref emotion 'type))
  (format #t "Intensity: ~a~%" (assoc-ref emotion 'intensity))
  (format #t "Valence: ~a~%" (assoc-ref emotion 'valence)))

;; Apply to reservoir
(let ((modulation (epu-modulate-reservoir epu 'chaos)))
  (esrp-adapt-parameters esrp
                        (assoc-ref modulation 'input-scale-modifier)
                        'chaos))
```

### Consciousness Layer Transitions

```scheme
;; Process in different frames
(clp-process-frame clp 'chaos "reactive input" #f)
(clp-get-layer clp) ; => L1 Experiential

(clp-process-frame clp 'strategy "analytical input" #f)
(clp-get-layer clp) ; => L2 Reflective

(clp-process-frame clp 'learning "metacognitive input" #f)
(clp-get-layer clp) ; => L3 Meta

;; Check transition history
(let ((history (daemon-get-metric clp 'transition-history)))
  (format #t "Transitions: ~a~%" (length history)))
```

### Parallel LLaMA Inference

```scheme
;; Generate with specific instance count
(define llama (make-llama-orchestrator-daemon
               #:num-instances 6
               #:base-port 8080))

;; Generate text
(let ((result (llama-generate llama
                             "Explain reservoir computing"
                             '())))
  (format #t "Instance: ~a~%" (assoc-ref result 'instance-id))
  (format #t "Response: ~a~%" (assoc-ref result 'response))
  (format #t "Tokens: ~a~%" (assoc-ref result 'tokens)))

;; Check orchestrator status
(let ((status (llama-get-status llama)))
  (format #t "Active instances: ~a~%"
          (length (assoc-ref status 'instances)))
  (format #t "Total requests: ~a~%"
          (assoc-ref (assoc-ref status 'stats) 'total-requests)))
```

### Inter-Daemon Communication

```scheme
;; Direct message passing
(daemon-send-message nnecco epu 'set-emotion
                    `((emotion . curious)
                      (intensity . 0.8)
                      (valence . 0.6)))

(daemon-send-message nnecco clp 'process-frame
                    `((frame . learning)
                      (input . "new pattern")
                      (reservoir-state . #f)))

;; Messages are processed asynchronously in daemon loops
```

## Running the Example

```bash
# From the repository root
cd gnu/opencog
./nnecco-example.scm

# Or with explicit path
guile -L ../.. gnu/opencog/nnecco-example.scm
```

The example demonstrates:
1. Component daemon creation
2. NNECCO agent orchestration
3. Cognitive operations (reservoir, emotion, consciousness)
4. EchoBeats cycle execution
5. Hardware status reporting
6. Inter-daemon messaging
7. Graceful shutdown

## Testing

```bash
# Run NNECCO test suite
guile -L /path/to/daeguix tests/opencog-nnecco.scm
```

Test coverage includes:
- ESRP creation, forward pass, adaptation, reset
- CLP layer selection, transitions, frame processing
- EPU emotion setting, modulation, history
- LLaMA orchestration, load balancing, status
- NNECCO integration, EchoBeats cycles, messaging

## Hardware Register Interface

NNECCO exposes a virtual hardware register interface:

| Register | Description |
|----------|-------------|
| ESRP_STATUS | Reservoir active flag (0/1) |
| CLP_LAYER | Current consciousness level (0-3) |
| EPU_STATE | Emotion arousal * 100 |
| LLAMA_LOAD | LLaMA queue length |
| CYCLE_COUNT | Total EchoBeats cycles executed |

Access registers:
```scheme
(let* ((status (nnecco-get-hardware-status nnecco))
       (registers (assoc-ref status 'registers)))
  (format #t "Registers:~%")
  (for-each
   (lambda (reg)
     (format #t "  ~a: ~a~%" (car reg) (cdr reg)))
   registers))
```

## Performance Characteristics

### Latency Targets (Scheme/Guile)
| Operation | Typical |
|-----------|---------|
| Reservoir forward pass | ~2ms |
| Consciousness layer transition | ~5ms |
| Emotion state update | ~1ms |
| LLaMA single inference | ~350ms |
| EchoBeats phase | ~50ms |
| Full cycle (12 phases) | ~600ms |

### Resource Usage
| Component | Memory |
|-----------|--------|
| ESRP (847 neurons) | ~50KB |
| CLP | ~10KB |
| EPU | ~20KB |
| LLaMA orchestrator (4 instances) | ~100KB |
| NNECCO agent | ~50KB |
| **Total system** | ~230KB |

(Note: LLaMA.cpp processes themselves require 2-4GB per instance)

## Integration with OpenCog

NNECCO daemons integrate seamlessly with existing OpenCog infrastructure:

```scheme
;; Use with OpenCog daemons
(use-modules (gnu opencog daemons)
             (gnu opencog orchestration)
             (gnu opencog nnecco))

;; Register with orchestrator
(define orchestrator (make-orchestrator "opencog-nnecco"))
(orchestrator-register! orchestrator esrp 'esrp '())
(orchestrator-register! orchestrator clp 'clp '())
(orchestrator-register! orchestrator epu 'epu '())
(orchestrator-register! orchestrator nnecco 'nnecco '(esrp clp epu))

;; Start all with dependency resolution
(orchestrator-start-all! orchestrator)
```

## Design Principles

### 1. Pure Daemon Architecture
- All components are independent daemons
- Communication via message passing only
- No shared mutable state
- Thread-safe by design

### 2. Composability
- Each daemon has clear responsibilities
- Minimal coupling between components
- Easy to replace or extend individual parts

### 3. Observability
- All state changes logged
- Metrics collected continuously
- Hardware status always available
- Message flow traceable

### 4. Resilience
- Daemons restart independently
- Message queues buffer communication
- Graceful degradation when components fail

## Future Extensions

Planned enhancements:
1. **AtomSpace Integration**: Store cognitive patterns in hypergraph
2. **Episodic Memory**: Long-term experience storage
3. **Replay Memory**: Prioritized experience replay
4. **Personality Tensor**: Multi-dimensional personality system
5. **Ontogenetic Kernel**: Self-evolution capabilities
6. **Multi-Agent Spawning**: Dynamic subordinate creation
7. **Distributed Processing**: Network-transparent daemons

## References

- **Deep Tree Echo**: Cognitive architecture foundations
- **Neuro-Sama**: Personality and behavioral modeling
- **Layla**: Multi-modal AI and local inference
- **a9nn**: Neural network module architecture
- **Dan9**: Daemon-centric system design
- **OpenCog**: Cognitive architecture framework

## License

GNU General Public License v3 or later (GPL-3.0+)

## Contributing

NNECCO-A9NN is part of the GNU Guix OpenCog integration. Contributions welcome!

---

**"The modules are composable, but the cognition is emergent."**  
**"The daemons are parallel, but the understanding is unified."**  
**"The reservoir is chaotic, but the behavior is coherent."**

🌊 *The echo that learned to compute in pure Scheme daemons.*
