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

### Running the Example

```bash
# Make sure guile is available
guile -L /path/to/daeguix gnu/dan9/example.scm

# Run egregore example
guile -L /path/to/daeguix gnu/dan9/egregore-example.scm

# Run antikythera example (time scaling)
guile -L /path/to/daeguix gnu/dan9/antikythera-example.scm
```

### Running Tests

```bash
# Core Dan9 tests
guile -L /path/to/daeguix tests/dan9.scm

# Egregore tests
guile -L /path/to/daeguix tests/egregore.scm

# Antikythera tests
guile -L /path/to/daeguix tests/antikythera.scm
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

## Future Directions

1. **Network Protocol**: 9P-like protocol for remote daemons
2. **Discovery**: Automatic daemon discovery
3. **Security**: Authentication and authorization
4. **Persistence**: State persistence and recovery
5. **Monitoring**: Real-time dashboard
6. **More Daemons**: Graphics, audio, storage, etc.

## Files

- `daemons.scm` - Core daemon infrastructure
- `filesystem.scm` - Filesystem daemon
- `network.scm` - Network daemon
- `process.scm` - Process management daemon
- `namespace.scm` - Namespace daemon
- `egregore.scm` - Daemon orchestration archetypes
- `antikythera.scm` - Nested event loop scheduler with time scaling
- `example.scm` - Basic example usage
- `egregore-example.scm` - Egregore orchestration example
- `antikythera-example.scm` - Time scaling scheduler example
- `../packages/dan9.scm` - Guix package definition
- `../../tests/dan9.scm` - Core test suite
- `../../tests/egregore.scm` - Egregore tests
- `../../tests/antikythera.scm` - Antikythera tests

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
