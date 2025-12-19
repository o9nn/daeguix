# Dan9: Everything is a Daemon

Dan9 is a daemon-centric architecture inspired by Plan9's file-centric philosophy. Where Plan9 says "everything is a file", Dan9 says "everything is a daemon".

## Philosophy

Plan9 provides uniform access to system resources through the filesystem namespace. Dan9 extends this philosophy to active services: all system resources and operations are exposed as daemons that communicate through message passing.

### Key Differences from Plan9

| Aspect | Plan9 | Dan9 |
|--------|-------|------|
| **Abstraction** | Everything is a file | Everything is a daemon |
| **Access** | Read/write operations | Message passing |
| **Protocol** | 9P filesystem protocol | Daemon message protocol |
| **State** | Files (passive) | Daemons (active) |
| **Communication** | File descriptors | Message queues |

## Core Components

### 1. Daemon Infrastructure (`daemons.scm`)

The foundation providing:
- Daemon lifecycle management
- Inter-daemon message passing
- Global daemon registry
- Thread-safe operations
- Metrics collection

### 2. Filesystem Daemon (`filesystem.scm`)

Handles file operations through messages:
- Read, write, list, stat
- Directory creation/deletion
- All operations via daemon messages

### 3. Network Daemon (`network.scm`)

Manages network operations:
- Connect, disconnect, send, receive
- Listen on ports
- Network I/O as daemon operations

### 4. Process Daemon (`process.scm`)

Process management through daemons:
- Spawn, kill, wait
- Process listing and tracking

### 5. Namespace Daemon (`namespace.scm`)

Daemon namespace management:
- Bind/unbind daemons to paths
- Mount/unmount services
- Namespace lookup

### 6. Egregore (`egregore.scm`)

Daemon orchestration archetypes:
- Collective group consciousness for coordinating daemons
- Four archetype patterns: swarm, hierarchy, ring, mesh
- Coordinator-based message routing
- Lifecycle management for daemon groups

### 7. Antikythera Mechanism (`antikythera.scm`)

Nested event loop scheduler with time scaling:
- Gear-based scheduling metaphor
- Hierarchical event loops at different time scales
- Time scaling (e.g., years -> days, days -> hours)
- Event registration and dispatch
- Inspired by the ancient Antikythera astronomical computer

### 8. Address System (`address.scm`)

URI-based daemon routing and thread pool management:
- D9 address protocol (d9://type/group/name)
- Address router for daemon resolution
- Thread pool management with instant cycle spawn/drop
- Synchronous cycle coordination (1 to 10^12 scalability)
- Replaces namespace lookups with direct addressing

### 9. Virtual Devices (`device.scm`)

Daemon-driven device abstractions:
- Clockwork devices: nested gearing arrays for complex timing
- Gesture devices: spatio-temporal vectors with intent recognition
- Device drivers: pluggable event handlers
- Egregore integration: devices initiated by orchestrators
- Pure daemon orchestrarchitecture

### 10. Persistence (`persistence.scm`)

State persistence and recovery for daemons:
- Daemon state serialization and deserialization
- Checkpoint and restore functionality
- Automatic periodic checkpoints
- Manual checkpoint control
- Configurable checkpoint policies

### 11. Monitoring (`monitoring.scm`)

Real-time daemon monitoring and dashboard:
- Metrics collection and history tracking
- Aggregate statistics (min, max, average)
- Text-based monitoring dashboard
- Daemon status visualization
- Metrics tables and reports

### 12. Logging (`logging.scm`)

Centralized logging for all daemons:
- Log levels (debug, info, warning, error, critical)
- Log filtering by level and source
- Log buffer with configurable size
- Log file output support
- Log entry formatting and display

### 13. Timer (`timer.scm`)

Scheduled task execution:
- One-time scheduled tasks
- Repeating scheduled tasks
- Task cancellation
- Task status monitoring
- Inter-daemon task coordination

## Usage

### Basic Example

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 filesystem)
             (gnu dan9 network))

;; Create daemons
(define fs (make-filesystem-daemon))
(define net (make-network-daemon))

;; Start daemons
(daemon-start fs filesystem-daemon-loop)
(daemon-start net network-daemon-loop)

;; Send messages
(define client (make-daemon "client" 'client))
(daemon-send-message client fs 'read '((path . "/data")))
(daemon-send-message client net 'connect '((host . "example.com") (port . 80)))

;; Check status
(daemon-status fs)
;; => ((name . "filesystem") (type . filesystem) (state . running) ...)

;; Get metrics
(daemon-get-metrics fs)
;; => ((read-count . 42) (write-count . 17) ...)
```

### Persistence Example

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 persistence))

;; Create persistence daemon
(define pers (make-persistence-daemon
              #:state-directory "/tmp/dan9-state"
              #:checkpoint-interval 300))
(daemon-start pers persistence-daemon-loop)

;; Create a worker daemon
(define worker (make-daemon "worker" 'worker))
(daemon-start worker worker-loop)

;; Do some work, accumulate state
(daemon-set-metric! worker 'tasks-completed 42)

;; Save state manually
(daemon-save-state worker #:directory "/tmp/dan9-state")

;; Or request persistence daemon to save
(daemon-send-message pers pers 'save '((daemon-name . "worker")))

;; Later, restore state
(let ((state (daemon-load-state worker #:directory "/tmp/dan9-state")))
  (format #t "Restored state: ~a~%" state))

;; Checkpoint all daemons
(daemon-send-message pers pers 'checkpoint-all '())
```

### Monitoring Example

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 monitoring))

;; Create monitoring daemon
(define monitor (make-monitoring-daemon #:collect-interval 5))
(daemon-start monitor monitoring-daemon-loop)

;; Display real-time dashboard
(daemon-send-message monitor monitor 'dashboard '())

;; Show specific daemon status
(daemon-send-message monitor monitor 'status
                    '((daemon-name . "worker")))

;; Display metrics table
(daemon-send-message monitor monitor 'metrics-table
                    '((metric-name . tasks-completed)))

;; Get metric history for analysis
(let ((history (monitor-history "worker" #:metric-name 'tasks-completed)))
  (format #t "Task history: ~a~%" history))

;; Compute aggregate statistics
(let ((agg (monitor-aggregate "worker" 'cpu-load)))
  (format #t "CPU stats: ~a~%" agg))
```

### Logging Example

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 logging))

;; Create logging daemon
(define logger (make-logging-daemon #:min-level 'info))
(daemon-start logger logging-daemon-loop)

;; Log messages from any daemon
(log-info "worker-1" "Task started")
(log-warning "worker-2" "High memory usage detected")
(log-error "worker-3" "Failed to process request")

;; Display recent logs
(daemon-send-message logger logger 'display '((limit . 20)))

;; Display filtered logs
(daemon-send-message logger logger 'display
                    '((level . warning) (limit . 10)))

;; Write logs to file
(daemon-send-message logger logger 'write-file
                    '((filepath . "/tmp/dan9.log")))
```

### Timer Example

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 timer))

;; Create timer daemon
(define timer (make-timer-daemon #:tick-interval 1))
(daemon-start timer timer-daemon-loop)

;; Schedule one-time task (send message in 10 seconds)
(schedule-task 10 "worker" 'process '((data . "batch-1")))

;; Schedule repeating task (every 30 seconds)
(schedule-repeating-task 30 "worker" 'heartbeat '())

;; List scheduled tasks
(list-scheduled-tasks)

;; Cancel a task
(let ((task-id (schedule-task 60 "worker" 'cleanup '())))
  (cancel-task task-id))

;; View timer statistics
(daemon-send-message timer timer 'stats '())
```

### Integrated System Example

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 persistence)
             (gnu dan9 monitoring)
             (gnu dan9 logging)
             (gnu dan9 timer))

;; Start all system daemons
(define logger (make-logging-daemon))
(define monitor (make-monitoring-daemon))
(define pers (make-persistence-daemon))
(define timer (make-timer-daemon))

(for-each daemon-start
          (list (list logger logging-daemon-loop)
                (list monitor monitoring-daemon-loop)
                (list pers persistence-daemon-loop)
                (list timer timer-daemon-loop)))

;; Create worker that uses all services
(define worker (make-daemon "worker" 'worker))
(daemon-start worker worker-loop)

;; Schedule periodic monitoring
(schedule-repeating-task 10 "monitoring" 'dashboard '())

;; Schedule periodic checkpoints
(schedule-repeating-task 300 "persistence" 'checkpoint-all '())

;; Log system events
(log-info "system" "Integrated Dan9 system started")

;; Monitor everything
(daemon-send-message timer monitor 'dashboard '())
```

### Running the Example

```bash
# Make sure guile is available
guile -L /path/to/daeguix gnu/dan9/example.scm

# Run egregore example
guile -L /path/to/daeguix gnu/dan9/egregore-example.scm

# Run antikythera example (time scaling)
guile -L /path/to/daeguix gnu/dan9/antikythera-example.scm

# Run address system & virtual devices example
guile -L /path/to/daeguix gnu/dan9/address-device-example.scm

# Run persistence example
guile -L /path/to/daeguix gnu/dan9/persistence-example.scm

# Run monitoring dashboard example
guile -L /path/to/daeguix gnu/dan9/monitoring-example.scm

# Run integrated system example (all new features)
guile -L /path/to/daeguix gnu/dan9/integrated-system-example.scm
```

### Running Tests

```bash
# Core Dan9 tests
guile -L /path/to/daeguix tests/dan9.scm

# Egregore tests
guile -L /path/to/daeguix tests/egregore.scm

# Antikythera tests
guile -L /path/to/daeguix tests/antikythera.scm

# Address system tests
guile -L /path/to/daeguix tests/address.scm

# Virtual device tests
guile -L /path/to/daeguix tests/device.scm

# Persistence tests
guile -L /path/to/daeguix tests/persistence.scm

# Monitoring tests
guile -L /path/to/daeguix tests/monitoring.scm

# Logging and timer tests
guile -L /path/to/daeguix tests/logging-timer.scm
```

## Architecture

### Message Protocol

All daemon communication uses structured messages:

```scheme
(define-record-type <daemon-message>
  (make-message sender recipient type payload timestamp)
  message?
  (sender message-sender)       ; Sender daemon name
  (recipient message-recipient) ; Recipient daemon name
  (type message-type)           ; Message type (e.g., 'read, 'write)
  (payload message-payload)     ; Operation data
  (timestamp message-timestamp)) ; Message timestamp
```

### Daemon Lifecycle

1. **Creation**: `make-daemon` creates daemon structure
2. **Registration**: Daemon registered in global registry (optional)
3. **Starting**: `daemon-start` launches daemon thread
4. **Running**: Loop function processes messages
5. **Stopping**: `daemon-stop` signals graceful shutdown

### Thread Safety

All operations are thread-safe:
- Daemon state changes use mutexes
- Message mailboxes are mutex-protected
- Registry access is synchronized
- Metrics updates are atomic

## Creating Custom Daemons

```scheme
(define (my-custom-loop daemon)
  "Custom daemon loop function"
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (handle-my-message daemon msg)))
      
      ;; Do custom work
      (do-custom-work daemon)
      
      ;; Update metrics
      (daemon-set-metric! daemon 'work-count
                         (+ 1 (or (daemon-get-metric daemon 'work-count) 0)))
      
      ;; Sleep briefly
      (usleep 10000)
      (loop))))

(define custom (make-daemon "my-service" 'custom
                           #:loop-fn my-custom-loop))
(daemon-start custom my-custom-loop)
```

## Egregore: Daemon Orchestration

Egregores are daemon orchestration archetypes - coordinated groups of daemons working toward a common goal. They provide four patterns:

### Swarm Archetype
All daemons receive broadcasts, no hierarchy. Best for parallel processing and worker pools.

```scheme
(use-modules (gnu dan9 egregore))

(define swarm (make-swarm-egregore "data-processors"))
(egregore-add-daemon! swarm worker1)
(egregore-add-daemon! swarm worker2)
(egregore-start swarm)
(egregore-broadcast swarm 'process '((data . "dataset.csv")))
```

### Hierarchy Archetype
Tree structure with coordinator distributing work to workers. Best for task delegation.

```scheme
(define hierarchy (make-hierarchy-egregore "job-system"
                                          #:daemons (list w1 w2 w3)))
(egregore-start hierarchy)
(egregore-broadcast hierarchy 'delegate '((job . "analyze-logs")))
```

### Ring Archetype
Circular message passing through daemons. Best for pipelines and token passing.

```scheme
(define ring (make-ring-egregore "pipeline"
                                #:daemons (list stage1 stage2 stage3)))
(egregore-start ring)
(egregore-broadcast ring 'circulate '((token . "process-token")))
```

### Mesh Archetype
Fully connected network where all daemons are aware of each other. Best for consensus and peer-to-peer coordination.

```scheme
(define mesh (make-mesh-egregore "consensus"
                                #:daemons (list node1 node2 node3)))
(egregore-start mesh)
(egregore-broadcast mesh 'sync-state '((state . "initial")))
```

## Antikythera: Time-Scaled Event Loops

The Antikythera mechanism is a nested event loop scheduler inspired by the ancient astronomical computer. It enables time scaling - simulating long periods in compressed time.

### Creating a Time-Scaled Scheduler

```scheme
(use-modules (gnu dan9 antikythera))

;; Create scheduler
(define scheduler (make-antikythera-daemon #:base-tick-ms 100))

;; Create nested gears (year -> day -> hour -> minute)
(define minute-gear (make-gear "minute" 1))
(define hour-gear (make-gear "hour" 60 #:parent-gear minute-gear))
(define day-gear (make-gear "day" 24 #:parent-gear hour-gear))
(define year-gear (make-gear "year" 365 #:parent-gear day-gear))

;; Register events
(register-event! day-gear "weekly" 7
                (lambda (gear tick)
                  (format #t "Week ~a completed~%" (/ tick 7))))

(register-event! year-gear "annual" 1
                (lambda (gear tick)
                  (format #t "Year ~a completed!~%" tick)))

;; Start scheduler and register gears
(antikythera-start scheduler)
(daemon-send-message scheduler scheduler 'register-gear `((gear . ,minute-gear)))
(daemon-send-message scheduler scheduler 'register-gear `((gear . ,year-gear)))
```

### Time Scaling Example

With the Antikythera mechanism, you can simulate years in seconds:
- 1 year (365 days) simulated in ~36 seconds (with 100ms base tick)
- Perfect for modeling long-term processes, astronomical cycles, or aging systems
- Nested event loops fire at different time scales simultaneously

## Address System: D9 Protocol

The D9 address system replaces Plan9-style namespaces with URI-based daemon routing, enabling instant cycle spawn/drop and thread pool synchronization.

### D9 Address Format

```scheme
(use-modules (gnu dan9 address))

;; D9 URI format: d9://type/group/name
(define router (make-address-router))

;; Register daemons with addresses
(router-register! router "d9://egregore/workers/worker-1" worker1)
(router-register! router "d9://scheduler/year-gear" year-gear)

;; Resolve addresses
(define addr (router-resolve router "d9://egregore/workers/worker-1"))
;; => <address> with daemon reference
```

### Thread Pool Management

Instant cycle spawn/drop from 1 to trillion synchronous cycles:

```scheme
;; Create thread pool
(define pool (make-thread-pool "event-loops" #:size 100))

;; Spawn 1000 execution cycles instantly
(thread-pool-sync-cycles! pool 1000)

;; Drop to 10 cycles instantly
(thread-pool-sync-cycles! pool 10)

;; Synchronize event loops and cron jobs to pool
(thread-pool-spawn-cycle! pool (lambda () (event-loop-worker)))
```

## Virtual Devices: Pure Daemon Orchestrarchitecture

Virtual devices are daemon-driven abstractions initiated by egregores, resembling nested clockwork gearing arrays with corresponding drivers.

### Gesture Device - Spatio-Temporal Vectors

```scheme
(use-modules (gnu dan9 device))

;; Create gesture device (touchpad-like with intent)
(define touchpad (make-gesture-device "touchpad"))
(device-start touchpad gesture-daemon-loop)

;; Record gesture with spatio-temporal properties
(gesture-record! touchpad 
                100 200  ; x, y position
                5        ; z (pressure)
                15       ; velocity
                '(1 0))  ; direction vector

;; Recognize intent from gesture history
(gesture-recognize touchpad)
;; => 'swipe, 'tap, or 'drag
```

### Clockwork Device - Nested Gearing Arrays

```scheme
;; Create clockwork device with nested gears
(define clockwork (make-clockwork-device "mechanism"))

;; Add interconnected gears (like Antikythera)
(clockwork-add-gear! clockwork (make-gear "seconds" 60))
(clockwork-add-gear! clockwork (make-gear "minutes" 60))
(clockwork-add-gear! clockwork (make-gear "hours" 24))

;; Tick the clockwork mechanism
(clockwork-tick! clockwork)
```

### Egregore-Device Integration

```scheme
;; Egregore initiates virtual devices
(define device-swarm (make-swarm-egregore "device-controllers"))

(define sensor1 (make-virtual-device "temp-sensor" 'sensor))
(define sensor2 (make-virtual-device "pressure-sensor" 'sensor))

;; Devices join egregore as daemons
(egregore-add-daemon! device-swarm (device-daemon sensor1))
(egregore-add-daemon! device-swarm (device-daemon sensor2))

(egregore-start device-swarm)

;; Broadcast to all devices
(egregore-broadcast device-swarm 'calibrate '((precision . high)))
```

## Benefits

1. **Location Transparency**: Daemons can run locally or remotely
2. **Uniform Interface**: All resources accessed the same way
3. **Easy Distribution**: Natural fit for distributed systems
4. **Isolation**: Each daemon has independent state
5. **Composability**: Daemons easily composed and chained
6. **Monitoring**: Built-in metrics for all daemons
7. **Security**: Message-based access control

## Comparison with Other Systems

### vs. Plan9
- Plan9: Passive files, synchronous I/O
- Dan9: Active daemons, asynchronous messages

### vs. Microservices
- Microservices: Network-based, HTTP/gRPC
- Dan9: Message-based, local or remote

### vs. Actor Model
- Actor Model: Pure message passing
- Dan9: Message passing + daemon lifecycle + namespace

## Implementation Notes

### Performance
- Daemons run in separate threads
- Message passing is asynchronous
- Minimal locking for thread safety
- 10ms polling interval (configurable)

### Memory
- Each daemon: ~few KB
- Message queue: grows with pending messages
- Metrics: hash table per daemon

### Scalability
- Limited by thread count
- Can distribute daemons across processes
- Future: network-transparent daemons

## New Features (2025)

Dan9 has been extended with several powerful new features:

### ✓ Persistence
- **State Management**: Save and restore daemon states
- **Checkpoints**: Automatic and manual checkpoint creation
- **Recovery**: Restore daemons from saved checkpoints
- **Policies**: Configurable checkpoint policies (interval, on-stop, on-change)

### ✓ Monitoring Dashboard
- **Real-time Metrics**: Collect and track daemon metrics over time
- **Dashboard**: Text-based monitoring dashboard with status visualization
- **Analytics**: Aggregate statistics (min, max, average, count)
- **History**: Metric history tracking with configurable buffer size

### ✓ Centralized Logging
- **Log Levels**: Debug, info, warning, error, critical
- **Filtering**: Filter logs by level and source
- **Storage**: In-memory buffer with optional file output
- **API**: Simple logging API for all daemons

### ✓ Task Scheduling
- **Scheduled Tasks**: Execute tasks at future times
- **Repeating Tasks**: Periodic task execution
- **Cancellation**: Cancel scheduled tasks
- **Coordination**: Schedule messages between daemons

## Future Directions

1. **Network Protocol**: 9P-like protocol for remote daemons
2. **Discovery**: Automatic daemon discovery
3. **Security**: Authentication and authorization
4. **More Daemons**: Graphics, audio, storage, etc.

## Files

### Core Infrastructure
- `daemons.scm` - Core daemon infrastructure
- `filesystem.scm` - Filesystem daemon
- `network.scm` - Network daemon
- `process.scm` - Process management daemon
- `namespace.scm` - Namespace daemon

### Advanced Features
- `egregore.scm` - Daemon orchestration archetypes
- `antikythera.scm` - Nested event loop scheduler with time scaling
- `address.scm` - D9 address system and thread pool management
- `device.scm` - Virtual devices (clockwork, gesture, sensors)

### New Features (2025)
- `persistence.scm` - State persistence and recovery
- `monitoring.scm` - Real-time monitoring and dashboard
- `logging.scm` - Centralized logging system
- `timer.scm` - Scheduled task execution

### Examples
- `example.scm` - Basic example usage
- `egregore-example.scm` - Egregore orchestration example
- `antikythera-example.scm` - Time scaling scheduler example
- `integrated-example.scm` - Combined egregore + antikythera
- `address-device-example.scm` - Address system + virtual devices
- `persistence-example.scm` - State persistence demonstration
- `monitoring-example.scm` - Monitoring dashboard demonstration
- `integrated-system-example.scm` - All new features working together

### Tests
- `../../tests/dan9.scm` - Core test suite
- `../../tests/egregore.scm` - Egregore tests
- `../../tests/antikythera.scm` - Antikythera tests
- `../../tests/address.scm` - Address system tests
- `../../tests/device.scm` - Virtual device tests
- `../../tests/persistence.scm` - Persistence tests
- `../../tests/monitoring.scm` - Monitoring tests
- `../../tests/logging-timer.scm` - Logging and timer tests

### Package
- `../packages/dan9.scm` - Guix package definition

## License

GNU General Public License v3 or later (GPL-3.0+)

## Contributing

Dan9 is part of the GNU Guix project. Contributions welcome!

## References

- [Plan9 from Bell Labs](https://9p.io/plan9/)
- [The Styx Architecture](http://doc.cat-v.org/plan_9/misc/styx/)
- [GNU Guix](https://guix.gnu.org/)

---

**Dan9**: Where everything becomes a daemon, and the system comes alive through message passing.
