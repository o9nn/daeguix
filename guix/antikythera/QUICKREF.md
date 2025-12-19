# AnTiKytHeRa Quick Reference

## Quick Start

```scheme
(use-modules (guix antikythera))

;; Create and start engine
(define engine (make-antikythera-engine #:worker-count 4))
(engine-start engine)

;; Perform recursive computation
(engine-recurse engine my-function '(arg1 arg2) #:mode 'auto)

;; Stop engine
(engine-stop engine)
```

## Temporal Flux Modes

### ChRoN (Sequential)
- **Use for**: Deterministic recursive descent, simple recursion
- **Characteristics**: Linear time progression, predictable
- **Example**: Binary tree traversal, factorial

```scheme
(engine-recurse engine factorial '(5) #:mode 'chron)
```

### KAiRoN (Opportunistic)
- **Use for**: Optimization problems, pruning opportunities
- **Characteristics**: Heuristic-driven, strategic branching
- **Example**: Path finding, game tree search

```scheme
(engine-recurse engine minimax '(board depth) #:mode 'kairon)
```

### AIoN (Cyclical)
- **Use for**: Problems with repeated subproblems, dynamic programming
- **Characteristics**: Memoization, cycle detection
- **Example**: Fibonacci, longest common subsequence

```scheme
(engine-recurse engine fibonacci '(40) #:mode 'aion)
```

### Auto Mode
- **Use for**: General purpose, when unsure
- **Characteristics**: Automatic mode selection based on patterns
- **Example**: Any recursive function

```scheme
(engine-recurse engine my-func args #:mode 'auto)
```

## API Reference

### Engine Operations

```scheme
;; Create engine
(make-antikythera-engine #:worker-count 4 
                         #:egregore-config config)

;; Start/stop engine
(engine-start engine)
(engine-stop engine)

;; Get engine state
(engine-state engine)
;; Returns: ((flux-mode . chron) (energy . 1.0) ...)

;; Transition flux mode
(transition-flux-mode engine 'kairon)

;; Detect cycles
(detect-cycle engine args)
```

### Recursion Operations

```scheme
;; Perform recursion in specific mode
(chron-recurse engine proc args context)
(kairon-recurse engine proc args context)
(aion-recurse engine proc args context)

;; General recursion with mode selection
(engine-recurse engine proc args #:mode 'auto)
```

### Tensor-Gears Operations

```scheme
(use-modules (guix antikythera tensor-gears))

;; Create tensor
(tensor-create #:shape '(3 3))

;; Matrix operations
(matrix-multiply matrix1 matrix2)
(matrix-compose matrix1 matrix2 matrix3)

;; Temporal operations
(advance-temporal-phase tensor)
(shift-operational-mode tensor)

;; Worker pool integration
(worker-pool->tensor pool)
(tensor->worker-pool tensor)
(synchronize-workers pool tensor)
```

### ATenCoRe Reactor Operations

```scheme
(use-modules (guix antikythera atencore))

;; Initialize reactor
(reactor-init)

;; State transitions
(reactor-state-transition reactor 'clock-advance 'chron)

;; Energy management
(reactor-compute-energy-cost operation state)
(reactor-allocate-budget state total-budget)
(reactor-optimize-energy state)

;; Event handling
(reactor-event-dispatch state event)
(reactor-run-event-loop state #:max-iterations 1000)
```

### Record Types

```scheme
(use-modules (guix antikythera records))

;; Temporal flux state
(make-temporal-flux-state #:mode 'chron
                          #:energy 1.0)

;; Recursion context
(make-recursion-context #:depth 0
                        #:parent #f
                        #:flux-mode 'chron)

;; Egregore configuration
(make-egregore-config #:chron-weight 0.4
                      #:kairon-weight 0.3
                      #:aion-weight 0.3)

;; Reactor state
(make-reactor-state #:energy-budget 1000.0
                    #:state-vector #(1.0 0.0 0.0))

;; Tensor state
(make-tensor-state #:shape '(3 3)
                   #:flux-mode 'chron)
```

## Sylvester Matrices

### Clock Matrix (Temporal Progression)
```
| 0  1  0 |
| 0  0  1 |
| 1  0  0 |
```
Advances: ChRoN → KAiRoN → AIoN → ChRoN

### Shift Matrix (State Transition)
```
| 0  0  1 |
| 1  0  0 |
| 0  1  0 |
```
Enables permutation of state components

## Energy Budget

Default allocation:
- Total: 1000.0 units
- ChRoN: 400.0 units (40%)
- KAiRoN: 300.0 units (30%)
- AIoN: 300.0 units (30%)

Costs:
- Clock advance: 10-20 units
- Shift mode: 20 units
- Recursion step: 5 units
- Cycle detection: 15 units

## C++ Virtual Hardware Driver

### Register Layout

**Control Registers (0x00-0x1F)**
- CMD_INIT (0x00)
- CMD_START (0x04)
- CMD_STOP (0x08)
- CMD_RECURSE (0x0C)
- CMD_TRANSITION (0x10)

**Status Registers (0x20-0x3F)**
- STATUS_FLAGS (0x20)
- STATUS_MODE (0x24)
- STATUS_DEPTH (0x28)
- STATUS_ENERGY (0x2C)

**Configuration Registers (0x40-0x5F)**
- CFG_WORKER_COUNT (0x40)
- CFG_CHRON_WEIGHT (0x44)
- CFG_KAIRON_WEIGHT (0x48)
- CFG_AION_WEIGHT (0x4C)

**Telemetry Registers (0x60-0x7F)**
- TELEM_CHRON_COUNT (0x60)
- TELEM_KAIRON_COUNT (0x64)
- TELEM_AION_COUNT (0x68)
- TELEM_CYCLE_COUNT (0x6C)

### C++ API

```cpp
#include "nix/nix-daemon/antikythera-driver.hh"

using namespace nix::antikythera;

// Create and initialize driver
AntikytheraDriver driver;
driver.probe();
driver.initialize();
driver.start();

// Register access
uint32_t mode = driver.read_register(DeviceRegister::STATUS_MODE);
driver.write_register(DeviceRegister::CMD_TRANSITION, 1);

// Matrix operations
StateVector state(1.0f, 0.0f, 0.0f);
StateVector new_state = driver.apply_clock_matrix(state);

// Energy management
driver.consume_energy(10.0f);
float energy = driver.get_energy_level();

// IPC
std::vector<uint8_t> data = {1, 2, 3};
driver.send_to_scheme(data);
auto response = driver.receive_from_scheme();

// Cleanup
driver.stop();
driver.shutdown();
```

## Common Patterns

### Pattern 1: Simple Recursion
```scheme
(define (process-list lst)
  (if (null? lst)
      '()
      (cons (process-item (car lst))
            (process-list (cdr lst)))))

(engine-recurse engine process-list (list my-list) #:mode 'chron)
```

### Pattern 2: Memoized Recursion
```scheme
(define (expensive-calc n)
  (if (< n 2)
      1
      (+ (expensive-calc (- n 1))
         (expensive-calc (- n 2)))))

(engine-recurse engine expensive-calc '(50) #:mode 'aion)
```

### Pattern 3: Optimization Search
```scheme
(define (find-optimal choices constraint)
  (if (satisfied? constraint)
      (current-solution)
      (try-branches choices)))

(engine-recurse engine find-optimal 
                (list choices constraint) 
                #:mode 'kairon)
```

## Troubleshooting

### Engine won't start
- Check that engine is created: `(antikythera-engine? engine)`
- Ensure engine-start is called: `(engine-start engine)`

### Low energy warnings
- Check reactor state: `(reactor-optimize-energy reactor)`
- Reallocate budget: `(reactor-allocate-budget reactor new-budget)`

### Unexpected mode selection in auto mode
- Inspect engine state: `(engine-state engine)`
- Use explicit mode: `#:mode 'chron` instead of `#:mode 'auto`
- Adjust egregore weights in configuration

### Poor performance
- Use AIoN mode for repeated subproblems
- Increase worker count: `#:worker-count 8`
- Profile energy consumption with telemetry

## See Also

- Full documentation: `guix/antikythera/README.md`
- Examples: `guix/antikythera/examples.scm`
- Tests: `tests/antikythera-recursion.scm`
- Source: `guix/antikythera.scm`
