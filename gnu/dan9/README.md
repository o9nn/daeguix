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
```

### Running Tests

```bash
guile -L /path/to/daeguix tests/dan9.scm
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
- `example.scm` - Example usage
- `../packages/dan9.scm` - Guix package definition
- `../../tests/dan9.scm` - Test suite

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
