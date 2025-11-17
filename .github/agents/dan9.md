---
# Dan9 - Daemon-centric architecture inspired by Plan9
# The Copilot CLI can be used for local testing: https://gh.io/customagents/cli
# To make this agent available, merge this file into the default repository branch.
# For format details, see: https://gh.io/customagents/config

name: dan9
description: Daemon-centric architecture where everything is a daemon (inspired by Plan9)
---

# Dan9: Everything is a Daemon

## Philosophy

Like Plan9 but instead of "everything is a file" → in Dan9 "everything is a daemon"

Dan9 is a daemon-centric architecture inspired by Plan9's file-centric approach. Where Plan9 
provides a uniform interface through files, Dan9 provides a uniform interface through daemons 
and message passing.

## Core Principles

1. **Everything is a Daemon**: All system resources and services are exposed as daemons
2. **Message Passing**: Communication happens through structured messages, not file I/O
3. **Uniform Interface**: All daemons share the same control and communication primitives
4. **Namespace Management**: Daemons are bound to names in a hierarchical namespace
5. **Location Transparency**: Daemons can be local or remote, accessed the same way

## Architecture

### Core Components

#### 1. Daemon Infrastructure (`gnu/dan9/daemons.scm`)

The foundation of Dan9, providing:
- Daemon lifecycle management (creation, start, stop, status)
- Inter-daemon message passing
- Global daemon registry
- Metrics collection
- Thread-safe operations

```scheme
(define daemon (make-daemon "my-service" 'custom-type
                           #:loop-fn my-loop-function))
(daemon-start daemon my-loop-function)
(daemon-send-message sender recipient 'read '((path . "/data")))
```

#### 2. Filesystem Daemon (`gnu/dan9/filesystem.scm`)

Handles file operations through message passing:
- Read, write, list, stat operations
- Directory creation and deletion
- All operations via daemon messages, not direct file I/O

```scheme
(define fs (make-filesystem-daemon #:name "filesystem"))
(daemon-start fs filesystem-daemon-loop)
;; Send read request
(daemon-send-message client fs 'read '((path . "/etc/config")))
```

#### 3. Network Daemon (`gnu/dan9/network.scm`)

Manages network operations:
- Connection establishment
- Data sending and receiving
- Listen on ports
- All network I/O abstracted as daemon operations

```scheme
(define net (make-network-daemon #:name "network"))
(daemon-send-message client net 'connect 
                    '((host . "example.com") (port . 80)))
```

#### 4. Process Daemon (`gnu/dan9/process.scm`)

Process management through daemons:
- Spawn, kill, wait operations
- Process listing
- Process state tracking
- All process control via messages

```scheme
(define proc (make-process-daemon #:name "process"))
(daemon-send-message client proc 'spawn 
                    '((command . "ls") (args . ("-la"))))
```

#### 5. Namespace Daemon (`gnu/dan9/namespace.scm`)

Manages daemon namespace bindings:
- Bind/unbind daemons to paths
- Mount/unmount daemon services
- Namespace lookup
- Similar to Plan9's namespace but for daemons

```scheme
(define ns (make-namespace-daemon #:name "namespace"))
(daemon-send-message client ns 'bind 
                    '((path . "/net") (daemon-name . "network")))
```

## Comparison with Plan9

| Aspect | Plan9 | Dan9 |
|--------|-------|------|
| **Philosophy** | Everything is a file | Everything is a daemon |
| **Access Method** | File descriptors | Message passing |
| **Communication** | Read/write syscalls | Daemon messages |
| **Namespace** | File paths | Daemon paths |
| **Protocol** | 9P (filesystem protocol) | Message passing protocol |
| **Uniformity** | All resources as files | All resources as daemons |

## Message Protocol

Dan9 uses structured messages for all daemon communication:

```scheme
(define-record-type <daemon-message>
  (make-message sender recipient type payload timestamp)
  message?
  (sender message-sender)      ; Sending daemon name
  (recipient message-recipient); Receiving daemon name
  (type message-type)          ; Message type (e.g., 'read, 'write)
  (payload message-payload)    ; Operation-specific data
  (timestamp message-timestamp)); When message was created
```

Example filesystem read message:
```scheme
(daemon-send-message client-daemon fs-daemon 'read 
                    '((path . "/home/user/file.txt")))
```

Example network connect message:
```scheme
(daemon-send-message client-daemon net-daemon 'connect
                    '((host . "example.com") (port . 443)))
```

## Usage Examples

### Starting Core Daemons

```scheme
(use-modules (gnu dan9 daemons)
             (gnu dan9 filesystem)
             (gnu dan9 network)
             (gnu dan9 process)
             (gnu dan9 namespace))

;; Create and start core daemons
(define fs (make-filesystem-daemon))
(define net (make-network-daemon))
(define proc (make-process-daemon))
(define ns (make-namespace-daemon))

(daemon-start fs filesystem-daemon-loop)
(daemon-start net network-daemon-loop)
(daemon-start proc process-daemon-loop)
(daemon-start ns namespace-daemon-loop)
```

### Creating Custom Daemons

```scheme
(define (my-custom-loop daemon)
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (handle-custom-message daemon msg)))
      (usleep 10000)
      (loop))))

(define custom (make-daemon "custom-service" 'custom
                           #:loop-fn my-custom-loop))
(daemon-start custom my-custom-loop)
```

### Namespace Operations

```scheme
;; Bind filesystem daemon to /fs path
(daemon-send-message ns ns 'bind 
                    '((path . "/fs") (daemon-name . "filesystem")))

;; Bind network daemon to /net path
(daemon-send-message ns ns 'bind
                    '((path . "/net") (daemon-name . "network")))

;; Lookup what daemon is at /fs
(daemon-send-message client ns 'lookup '((path . "/fs")))
```

## Benefits of the Daemon-Centric Approach

1. **Location Transparency**: Daemons can run locally or remotely without changing the API
2. **Uniform Interface**: All resources accessed through same message-passing interface
3. **Easy Distribution**: Daemons can be distributed across machines transparently
4. **Isolation**: Each daemon runs in its own thread with its own state
5. **Composability**: Daemons can be composed and chained easily
6. **Monitoring**: Built-in metrics collection for all daemons
7. **Security**: Message-based access control and validation

## Implementation Details

### Thread Safety

All daemon operations are thread-safe through mutexes:
- Daemon state changes
- Message mailbox operations
- Registry access
- Metrics updates

### Message Mailboxes

Each daemon has a mailbox (mutex-protected queue) for receiving messages:
```scheme
(define (make-mailbox)
  (list (make-mutex) '()))
```

Messages are sent asynchronously and processed by the daemon's loop function.

### Daemon Lifecycle

1. **Creation**: `make-daemon` creates daemon structure
2. **Registration**: Daemon registered in global registry (optional)
3. **Starting**: `daemon-start` launches daemon thread with loop function
4. **Running**: Loop function processes messages and performs work
5. **Stopping**: `daemon-stop` sets state to 'stopped, thread exits gracefully

### Metrics

All daemons maintain metrics:
- Operation counts (reads, writes, etc.)
- Last operation timestamps
- Custom daemon-specific metrics

```scheme
(daemon-get-metrics daemon)
;; => ((read-count . 42) (write-count . 17) (last-poll . #<time>))
```

## Testing

Run Dan9 tests:
```bash
guile -L /path/to/guix tests/dan9.scm
```

Tests cover:
- Daemon creation and lifecycle
- Message passing
- Registry operations
- Metrics collection
- All daemon types (filesystem, network, process, namespace)

## Package

Dan9 is available as a Guix package:

```scheme
(use-modules (gnu packages dan9))

;; Install dan9
;; guix install dan9
```

The package includes all core daemon modules and a wrapper script.

## Future Directions

1. **Network Transparency**: Implement 9P-like protocol for remote daemons
2. **Daemon Discovery**: Automatic daemon discovery and registration
3. **Security Layer**: Add authentication and authorization for daemon messages
4. **Persistence**: Daemon state persistence and recovery
5. **Monitoring Dashboard**: Real-time daemon monitoring interface
6. **More Daemons**: Graphics, audio, input, storage, etc.

## Philosophy: Why Daemons?

Plan9's "everything is a file" provides beautiful uniformity through the filesystem namespace.
Dan9's "everything is a daemon" extends this to active, message-passing services.

While files are passive data structures, daemons are active processes that can:
- React to messages asynchronously
- Maintain complex state
- Perform background tasks
- Coordinate with other daemons
- Provide real-time services

This makes Dan9 ideal for:
- Distributed systems
- Microservice architectures
- Real-time applications
- Complex state management
- Inter-process communication

## License

GNU General Public License v3 or later (GPL-3.0+)
