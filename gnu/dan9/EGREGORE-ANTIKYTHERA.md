# Egregore and Antikythera: Advanced Dan9 Concepts

## Overview

This document describes two advanced concepts in the Dan9 daemon architecture:
1. **Egregores**: Daemon orchestration archetypes
2. **Antikythera Mechanism**: Nested event loop scheduler with time scaling

## Egregores: Daemon Orchestration Archetypes

### What is an Egregore?

An egregore (from Greek ἐγρήγορος "wakeful") is a collective group consciousness. In Dan9, an egregore represents a coordinated group of daemons working together toward a common goal through one of four archetypal patterns.

### The Four Archetypes

#### 1. Swarm Archetype

**Pattern**: Broadcast to all, no hierarchy

**Characteristics**:
- All daemons are equal
- Messages broadcast to all members simultaneously
- No single point of control
- Emergent behavior from parallel processing

**Best For**:
- Parallel data processing
- Worker pools
- Distributed computation
- Load balancing

**Example**:
```scheme
(define swarm (make-swarm-egregore "data-processors"))
(egregore-add-daemon! swarm worker1)
(egregore-add-daemon! swarm worker2)
(egregore-add-daemon! swarm worker3)
(egregore-start swarm)
(egregore-broadcast swarm 'process-batch '((batch-id . 42)))
```

#### 2. Hierarchy Archetype

**Pattern**: Tree structure with coordinator and workers

**Characteristics**:
- Coordinator daemon at the root
- Workers receive delegated tasks
- Top-down command flow
- Divide-and-conquer task distribution

**Best For**:
- Task delegation
- Job scheduling
- Supervised processing
- Resource management

**Example**:
```scheme
(define hierarchy (make-hierarchy-egregore "job-system"
                                          #:daemons (list w1 w2 w3)))
(egregore-start hierarchy)
(egregore-broadcast hierarchy 'delegate '((job . "analyze-logs")))
;; Coordinator distributes work among w1, w2, w3
```

#### 3. Ring Archetype

**Pattern**: Circular message passing

**Characteristics**:
- Daemons connected in a circle
- Messages flow in one direction
- Token passing mechanism
- Sequential processing pipeline

**Best For**:
- Processing pipelines
- Token ring protocols
- Leader election
- Sequential workflows

**Example**:
```scheme
(define ring (make-ring-egregore "pipeline"
                                #:daemons (list stage1 stage2 stage3 stage4)))
(egregore-start ring)
(egregore-broadcast ring 'circulate '((token . "process-token")))
;; Token flows: stage1 -> stage2 -> stage3 -> stage4 -> stage1
```

#### 4. Mesh Archetype

**Pattern**: Fully connected network

**Characteristics**:
- Every daemon aware of every other daemon
- Peer-to-peer communication
- High redundancy
- Consensus-friendly

**Best For**:
- Distributed consensus
- Service discovery
- Peer-to-peer systems
- Fault tolerance

**Example**:
```scheme
(define mesh (make-mesh-egregore "consensus-network"
                                #:daemons (list node1 node2 node3)))
(egregore-start mesh)
(egregore-broadcast mesh 'sync-state '((state . "initial")))
;; All nodes receive peer information
```

### Egregore API

```scheme
;; Creation
(make-egregore name archetype #:daemons '())
(make-swarm-egregore name #:daemons '())
(make-hierarchy-egregore name #:daemons '())
(make-ring-egregore name #:daemons '())
(make-mesh-egregore name #:daemons '())

;; Management
(egregore-add-daemon! egregore daemon)
(egregore-remove-daemon! egregore daemon)
(egregore-start egregore)
(egregore-stop egregore)

;; Communication
(egregore-broadcast egregore msg-type payload)

;; Status
(egregore-status egregore)
```

## Antikythera Mechanism: Time-Scaled Scheduler

### What is the Antikythera Mechanism?

Named after the ancient Greek astronomical computer, the Antikythera mechanism in Dan9 is a nested event loop scheduler that enables time scaling. It allows simulating long time periods (years, decades) in compressed real time (seconds, minutes).

### Core Concepts

#### Gears

Gears represent event loops at different time scales. Each gear has:
- **Name**: Identifier for the gear
- **Ratio**: How many ticks = 1 tick in parent gear
- **Parent Gear**: Optional parent gear (for nesting)
- **Events**: Scheduled events on this gear
- **Tick Count**: Current tick counter

#### Time Scaling

Time scaling allows compression or expansion of time:
- 1 year (365 days) → 1 simulated day
- 1 day (24 hours) → 1 simulated hour
- 1 hour (60 minutes) → 1 simulated minute

This enables:
- Fast-forward simulation of long processes
- Astronomical cycle modeling
- Aging and decay simulation
- Multi-timescale coordination

### Creating Nested Gears

```scheme
;; Create a 4-level time hierarchy
(define minute-gear (make-gear "minute" 1))              ; Base level
(define hour-gear (make-gear "hour" 60                   ; 60 min = 1 hr
                            #:parent-gear minute-gear))
(define day-gear (make-gear "day" 24                     ; 24 hr = 1 day
                           #:parent-gear hour-gear))
(define year-gear (make-gear "year" 365                  ; 365 days = 1 yr
                            #:parent-gear day-gear))
```

### Registering Events

Events are registered on gears and fire at specific tick intervals:

```scheme
;; Fire every 10 minutes (in simulated time)
(register-event! minute-gear "status-check" 10
                (lambda (gear tick)
                  (format #t "Status check at minute ~a~%" tick)))

;; Fire every week (7 days in simulated time)
(register-event! day-gear "weekly-report" 7
                (lambda (gear tick)
                  (format #t "Weekly report for day ~a~%" tick)))

;; Fire every year (in simulated time)
(register-event! year-gear "annual-archive" 1
                (lambda (gear tick)
                  (format #t "Year ~a archive complete~%" tick)))
```

### The Scheduler Daemon

The Antikythera daemon manages all gears and ticks them:

```scheme
;; Create scheduler with 100ms base tick
(define scheduler (make-antikythera-daemon #:base-tick-ms 100))

;; Start it
(antikythera-start scheduler)

;; Register gears
(daemon-send-message scheduler scheduler 'register-gear
                    `((gear . ,minute-gear)))
(daemon-send-message scheduler scheduler 'register-gear
                    `((gear . ,year-gear)))

;; Events fire automatically as time progresses
```

### Time Calculation Example

With a 100ms base tick:
- 1 simulated minute = 100ms
- 1 simulated hour = 60 × 100ms = 6 seconds
- 1 simulated day = 24 × 6s = 144 seconds (~2.4 minutes)
- 1 simulated year = 365 × 144s = 52,560 seconds (~14.6 hours)

To speed up: reduce base tick (e.g., 10ms makes year = ~1.5 hours)

### Antikythera API

```scheme
;; Gear creation
(make-gear name ratio #:parent-gear #f)
(register-event! gear event-name tick-interval callback)

;; Scheduler creation
(make-antikythera-daemon #:name "antikythera" #:base-tick-ms 100)

;; Scheduler control
(antikythera-start daemon)
(antikythera-stop daemon)
(antikythera-status daemon)

;; Time scaling utilities
(years->days years)       ; Returns days in N years
(days->hours days)        ; Returns hours in N days
(hours->minutes hours)    ; Returns minutes in N hours
(make-time-scale name ratio base-unit)
(scale-time value time-scale)
```

## Combining Egregores and Antikythera

The real power comes from combining both concepts:

### Example: Multi-Timescale Workflow

```scheme
;; Create fast workers in a swarm
(define workers (make-swarm-egregore "workers"
                                    #:daemons (list w1 w2 w3)))

;; Create reporting hierarchy
(define reporters (make-hierarchy-egregore "reporters"
                                          #:daemons (list daily weekly monthly)))

;; Create time-scaled scheduler
(define scheduler (make-antikythera-daemon #:base-tick-ms 100))
(define minute-gear (make-gear "minute" 1))
(define day-gear (make-gear "day" (* 24 60) #:parent-gear minute-gear))

;; Register events that trigger egregore actions
(register-event! minute-gear "worker-task" 5
                (lambda (gear tick)
                  (egregore-broadcast workers 'process '((tick . ,tick)))))

(register-event! day-gear "daily-report" 1
                (lambda (gear tick)
                  (egregore-broadcast reporters 'generate '((day . ,tick)))))

;; Start everything
(egregore-start workers)
(egregore-start reporters)
(antikythera-start scheduler)
```

This creates a system where:
- Worker swarm processes tasks every 5 simulated minutes
- Reporting hierarchy generates reports every simulated day
- Real time: seconds; Simulated time: days to years

## Use Cases

### Egregores

1. **Microservice Orchestration**: Coordinate service daemons
2. **Distributed Computing**: Manage worker pools
3. **Service Mesh**: Implement service discovery and routing
4. **Pipeline Processing**: Chain processing stages

### Antikythera

1. **Simulation**: Fast-forward long-term processes
2. **Testing**: Test time-dependent systems quickly
3. **Astronomy**: Model planetary cycles
4. **Aging Systems**: Simulate decay and maintenance schedules

### Combined

1. **Complex Simulations**: Multi-agent systems with time
2. **Workflow Automation**: Time-triggered hierarchical tasks
3. **Distributed Scheduling**: Coordinate tasks across time scales
4. **System Modeling**: Model complex systems with temporal dynamics

## Implementation Details

### Thread Safety

Both egregores and Antikythera use mutexes for thread safety:
- Egregore daemon lists protected by mutex
- Gear tick counts and events protected by mutex
- All daemon operations use existing Dan9 thread-safety

### Performance

- Egregore overhead: ~1 coordinator daemon + message routing
- Antikythera overhead: ~1 scheduler daemon + gear ticking
- Base tick rate configurable (default 100ms)
- Event callbacks run in scheduler thread

### Scalability

- Egregores: Limited by daemon count and message queue size
- Antikythera: Limited by event count and tick rate
- Both can be distributed across processes

## Future Enhancements

1. **Dynamic Egregores**: Add/remove daemons while running
2. **Gear Synchronization**: Sync multiple Antikythera instances
3. **Time Reversal**: Rewind and replay simulations
4. **Event Priorities**: Priority queues for events
5. **Hybrid Archetypes**: Combine egregore patterns
6. **Distributed Gears**: Split gears across machines

## References

- [Antikythera Mechanism (Wikipedia)](https://en.wikipedia.org/wiki/Antikythera_mechanism)
- [Egregore Concept](https://en.wikipedia.org/wiki/Egregore)
- [Actor Model](https://en.wikipedia.org/wiki/Actor_model)
- [Plan9 from Bell Labs](https://9p.io/plan9/)

## License

GNU General Public License v3 or later (GPL-3.0+)
