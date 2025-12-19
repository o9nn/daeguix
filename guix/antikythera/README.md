# AnTiKytHeRa Nested Recursion Engine

## Overview

The **AnTiKytHeRa** (Ancient Timekeeping Recursive Apparatus) is a daemon-based nested recursion engine that implements a triadic egregore system for sophisticated temporal flux control in recursive computations. It is designed as a virtual hardware device with associated drivers, integrating seamlessly with the Guix package manager infrastructure.

## Architecture

### Triadic Egregore System

AnTiKytHeRa implements three masters of temporal flux, each representing a distinct mode of time perception and progression:

1. **ChRoN (Chronos)** - Sequential, linear time progression
   - Best for deterministic recursive descent
   - Provides predictable, step-by-step recursion
   - Ideal for algorithms with clear recursive structure

2. **KAiRoN (Kairos)** - Opportune time, optimal moments
   - Enables heuristic-driven recursion with pruning
   - Recognizes optimal branching opportunities
   - Best for optimization problems requiring strategic decisions

3. **AIoN (Aion)** - Cyclical, eternal time patterns
   - Implements memoization and cycle detection
   - Leverages previously computed results
   - Ideal for problems with repeated subproblems (dynamic programming)

### Core Components

#### 1. Scheme Modules (guix/antikythera/)

- **records.scm** - Fundamental record types
  - `temporal-flux-state`: Tracks current temporal mode and energy
  - `recursion-context`: Captures recursion depth and state
  - `egregore-config`: Configuration for the three flux masters
  - `reactor-state`: ATenCoRe reactor state and energy budgets
  - `tensor-state`: Worker pool state as tensor representation

- **tensor-gears.scm** - Thread pool synchronization via tensor operations
  - Implements Sylvester's Clock Matrix (3×3 circulant matrix)
  - Implements Sylvester's Shift Matrix (3×3 permutation matrix)
  - Adapts ATen (PyTorch) tensor concepts for Guix worker pools
  - Provides matrix-based state transitions

- **atencore.scm** - ATenCoRe (ATen Core Reactor)
  - Reactor pattern implementation for event-driven state management
  - Energy tracking and budget allocation across temporal modes
  - State transitions using Sylvester matrix operations
  - Event loop for asynchronous state updates

- **antikythera.scm** - Main recursion engine
  - Daemon-based architecture for persistent service
  - Integration with guix/workers.scm thread pools
  - Automatic mode selection based on recursion patterns
  - Cycle detection and memoization

#### 2. C++ Virtual Hardware Driver (nix/nix-daemon/)

- **antikythera-driver.hh/cc** - Virtual hardware abstraction
  - Memory-mapped I/O (MMIO) register interface
  - Sylvester matrix operations in C++ for performance
  - IPC protocol for Scheme-C++ communication
  - Integration with Nix/Guix daemon infrastructure
  - Telemetry and energy tracking

### Mathematical Foundation

#### Sylvester Clock Matrix

The Clock Matrix is a 3×3 circulant matrix that advances temporal state through the three flux modes:

```
Clock = | 0  1  0 |
        | 0  0  1 |
        | 1  0  0 |
```

Matrix multiplication by the Clock Matrix produces the transition:
- ChRoN → KAiRoN → AIoN → ChRoN (cyclic progression)

#### Sylvester Shift Matrix

The Shift Matrix is a 3×3 permutation matrix for state transitions:

```
Shift = | 0  0  1 |
        | 1  0  0 |
        | 0  1  0 |
```

Enables direct transitions between operational modes through permutation.

#### State Vector Representation

The system state is represented as a 3D vector in temporal flux space:

```
State = [ChRoN, KAiRoN, AIoN]^T
```

Where each component represents the activation level (0.0-1.0) of that temporal mode.

## Usage

### Basic Example

```scheme
(use-modules (guix antikythera))

;; Create and start the engine
(define engine (make-antikythera-engine #:worker-count 4))
(engine-start engine)

;; Perform a recursive computation
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;; Use ChRoN mode for sequential recursion
(engine-recurse engine fibonacci '(10) #:mode 'chron)

;; Use AIoN mode for memoization
(engine-recurse engine fibonacci '(40) #:mode 'aion)

;; Auto mode selects optimal flux based on patterns
(engine-recurse engine fibonacci '(30) #:mode 'auto)

;; Stop the engine
(engine-stop engine)
```

### Temporal Flux Mode Selection

```scheme
;; Explicit mode selection
(engine-recurse engine my-function args #:mode 'chron)   ; Sequential
(engine-recurse engine my-function args #:mode 'kairon)  ; Opportunistic
(engine-recurse engine my-function args #:mode 'aion)    ; Cyclical/Memoized

;; Automatic mode selection (recommended)
(engine-recurse engine my-function args #:mode 'auto)
```

### Querying Engine State

```scheme
;; Get current engine state
(define state (engine-state engine))

;; Extract specific information
(assoc-ref state 'flux-mode)      ; Current temporal mode
(assoc-ref state 'energy)         ; Energy level
(assoc-ref state 'context-depth)  ; Recursion depth
(assoc-ref state 'running)        ; Running status
```

### Using Tensor-Gears for Synchronization

```scheme
(use-modules (guix antikythera tensor-gears))

;; Create a tensor state
(define tensor (tensor-create #:shape '(3 3)))

;; Apply Clock Matrix for temporal progression
(define advanced (advance-temporal-phase tensor))

;; Apply Shift Matrix for mode transition
(define shifted (shift-operational-mode tensor))

;; Synchronize worker pool with tensor state
(synchronize-workers pool tensor)
```

### ATenCoRe Reactor Operations

```scheme
(use-modules (guix antikythera atencore))

;; Initialize reactor
(define reactor (reactor-init))

;; Perform state transition
(define new-reactor (reactor-state-transition reactor 'clock-advance 'chron))

;; Check energy optimization opportunities
(define recommendations (reactor-optimize-energy reactor))

;; Run event loop
(define final-reactor (reactor-run-event-loop reactor))
```

## Integration with Guix

The AnTiKytHeRa engine integrates with existing Guix infrastructure:

1. **Worker Pools** - Uses `guix/workers.scm` for thread management
2. **Record Types** - Follows `guix/records.scm` patterns
3. **Daemon Architecture** - Extends Nix daemon in `nix/nix-daemon/`
4. **Functional Programming** - Immutable data structures and pure functions

## Performance Considerations

- **ChRoN Mode**: Lowest overhead, predictable performance
- **KAiRoN Mode**: Medium overhead, performance depends on heuristics
- **AIoN Mode**: Higher initial overhead, faster for repeated computations
- **Auto Mode**: Adapts overhead based on detected patterns

## Energy Model

The ATenCoRe reactor tracks computational energy:

- **Total Budget**: 1000.0 units (default)
- **ChRoN Budget**: 40% (400.0 units) - Linear operations
- **KAiRoN Budget**: 30% (300.0 units) - Opportunistic operations
- **AIoN Budget**: 30% (300.0 units) - Cyclical operations

Energy is consumed during:
- State transitions (10-20 units)
- Recursion steps (5 units)
- Cycle detection (15 units)

## Virtual Hardware Interface

The C++ driver exposes a register-based interface:

### Control Registers
- `CMD_INIT` (0x00): Initialize engine
- `CMD_START` (0x04): Start engine
- `CMD_STOP` (0x08): Stop engine
- `CMD_RECURSE` (0x0C): Trigger recursion
- `CMD_TRANSITION` (0x10): Change flux mode

### Status Registers
- `STATUS_FLAGS` (0x20): Initialization and running flags
- `STATUS_MODE` (0x24): Current temporal flux mode
- `STATUS_DEPTH` (0x28): Current recursion depth
- `STATUS_ENERGY` (0x2C): Energy level

### Telemetry Registers
- `TELEM_CHRON_COUNT` (0x60): ChRoN invocations
- `TELEM_KAIRON_COUNT` (0x64): KAiRoN invocations
- `TELEM_AION_COUNT` (0x68): AIoN invocations
- `TELEM_CYCLE_COUNT` (0x6C): Detected cycles

## Testing

```scheme
;; Example test cases
(test-begin "antikythera-engine")

(test-assert "engine-creation"
  (antikythera-engine? (make-antikythera-engine)))

(test-equal "engine-start"
  #t
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (assoc-ref (engine-state engine) 'running)))

(test-equal "chron-mode-recursion"
  55
  (let ((engine (make-antikythera-engine)))
    (engine-start engine)
    (engine-recurse engine fibonacci '(10) #:mode 'chron)))

(test-end "antikythera-engine")
```

## Future Enhancements

1. **Distributed Recursion**: Extend to multiple nodes
2. **GPU Acceleration**: Offload matrix operations to GPU
3. **Advanced Heuristics**: ML-based mode selection
4. **Visualization**: Real-time temporal flux visualization
5. **Persistence**: Save/restore recursion contexts

## References

- Sylvester, J.J. (1884). "On the Equation to the Secular Inequalities in the Planetary Theory"
- ATen: PyTorch C++ Tensor Library - https://github.com/pytorch/pytorch
- Reactor Pattern - Douglas C. Schmidt
- Guix Manual - https://guix.gnu.org/manual/

## License

Copyright © 2025 AnTiKytHeRa Project

This is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

See the file COPYING in the top-level directory of the Guix distribution or <http://www.gnu.org/licenses/>.
