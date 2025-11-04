# OpenCog Autonomous Multi-Agent Orchestration - Technical Documentation

## Table of Contents

1. [Introduction](#introduction)
2. [Architecture Overview](#architecture-overview)
3. [Implementation Details](#implementation-details)
4. [Configuration Guide](#configuration-guide)
5. [Development Guide](#development-guide)
6. [API Reference](#api-reference)
7. [Troubleshooting](#troubleshooting)

## Introduction

This implementation transforms OpenCog into an autonomous multi-agent orchestration workbench, built entirely on daemon-based architecture using GNU Shepherd and implemented in pure Scheme wherever possible.

### Key Design Principles

1. **Daemon-First Architecture**: Every component runs as a supervised daemon
2. **Pure Scheme Implementation**: All orchestration logic in Scheme (no external scripts)
3. **Declarative Configuration**: Entire system configured through Scheme records
4. **Automatic Dependency Management**: Agents start only when dependencies are ready
5. **Fault Tolerance**: Automatic restart and error recovery via Shepherd

## Architecture Overview

### Component Hierarchy

```
OpenCog Orchestration Service (Shepherd Daemon)
├── AtomSpace (Shared Knowledge Store)
├── CogServer (Network Interface)
├── Attention System (Resource Allocation)
└── Agent Registry
    ├── Reasoning Agent (daemon thread)
    ├── Learning Agent (daemon thread)
    ├── Attention Agent (daemon thread)
    ├── Planning Agent (daemon thread)
    └── Communication Agent (daemon thread)
```

### Service Layer

**File**: `gnu/services/opencog.scm`

The service layer provides:
- Service type definitions for Shepherd integration
- Configuration records for type-safe configuration
- Shepherd service generators
- Agent lifecycle management

### Agent Framework

**File**: `gnu/opencog/agents.scm`

Pure Scheme implementations of cognitive agents:
- Reasoning Agent: Logical inference over AtomSpace
- Learning Agent: Pattern discovery and mining
- Attention Agent: Cognitive resource allocation
- Planning Agent: Action sequence generation
- Communication Agent: Inter-agent coordination

### Orchestration Core

**File**: `gnu/opencog/orchestration.scm`

Pure Scheme orchestrator providing:
- Thread-safe agent registry
- Dependency resolution
- State management
- Health monitoring

## Implementation Details

### Daemon Architecture

The orchestration service runs as a Shepherd daemon with the following properties:

```scheme
(shepherd-service
 (provision '(opencog-orchestration))
 (documentation "OpenCog autonomous multi-agent orchestration daemon")
 (requirement '(networking))
 (start #~(make-forkexec-constructor ...))
 (stop #~(make-kill-destructor)))
```

**Key Features**:
- Automatic restart on failure (via Shepherd respawn)
- Dependency on networking service
- Centralized logging to `/var/log/opencog-orchestration.log`
- Environment setup for Guile module paths

### Thread Safety

All shared state is protected by mutexes:

```scheme
(define orchestration-mutex (make-mutex))

(define (register-agent name agent-proc dependencies)
  (with-mutex orchestration-mutex
    (hash-set! agent-registry name ...)
    (hash-set! agent-states name 'registered)))
```

**Protected Resources**:
- Agent registry (hash table)
- Agent states (hash table)
- State transitions

### Agent Lifecycle

States: `registered` → `starting` → `running` → `stopping` → `stopped`

```
┌─────────────┐
│ registered  │
└──────┬──────┘
       │ start-agent called
       ▼
┌─────────────┐
│  starting   │ (check dependencies)
└──────┬──────┘
       │ dependencies met
       ▼
┌─────────────┐
│   running   │ (thread executing)
└──────┬──────┘
       │ stop-agent called
       ▼
┌─────────────┐
│  stopping   │
└──────┬──────┘
       │ cleanup complete
       ▼
┌─────────────┐
│   stopped   │
└─────────────┘
```

### Dependency Management

Dependencies are checked before agent startup:

```scheme
(if (every (lambda (dep)
            (eq? 'running (hash-ref agent-states dep)))
          dependencies)
    ;; Start agent
    (begin ...)
    ;; Defer startup
    #f)
```

This ensures agents only start when their dependencies are running.

### Communication Patterns

**Shared AtomSpace**:
- All agents have access to the same AtomSpace instance
- Knowledge sharing through atom manipulation
- Thread-safe operations via AtomSpace API

**State Monitoring**:
- Central orchestration loop monitors all agent states
- Error detection and logging
- Health checks every 5 seconds

## Configuration Guide

### Basic Configuration

```scheme
(use-modules (gnu services opencog))

(service opencog-orchestration-service-type
         (opencog-orchestration-configuration
          (port 17001)
          (log-level "INFO")))
```

### Adding Agents

```scheme
(opencog-orchestration-configuration
 (agents
  (list
   (opencog-agent-configuration
    (name 'my-agent)
    (module '(my custom module))
    (auto-start? #t)
    (dependencies '(other-agent))))))
```

### Configuration Options

**opencog-orchestration-configuration**:
- `atomspace`: Package for AtomSpace (default: atomspace)
- `cogserver`: Package for CogServer (default: cogserver)
- `attention`: Package for attention system (default: attention)
- `port`: CogServer port (default: 17001)
- `log-level`: Logging level (default: "INFO")
- `data-directory`: Data storage path (default: "/var/lib/opencog")
- `agents`: List of agent configurations (default: '())

**opencog-agent-configuration**:
- `name`: Agent identifier (required)
- `module`: Scheme module containing agent (required)
- `entry-point`: Agent procedure (optional)
- `auto-start?`: Start automatically (default: #t)
- `dependencies`: List of required agents (default: '())

## Development Guide

### Creating Custom Agents

1. **Define Agent Procedure**:

```scheme
(define (my-custom-agent atomspace)
  (let loop ((iteration 0))
    (format #t "Agent iteration ~a~%" iteration)
    ;; Agent logic here
    (sleep 10)
    (loop (+ iteration 1))))
```

2. **Create Agent Module**:

```scheme
(define-module (my custom agents)
  #:export (my-custom-agent))

(define (my-custom-agent atomspace)
  ...)
```

3. **Register in Configuration**:

```scheme
(opencog-agent-configuration
 (name 'my-custom-agent)
 (module '(my custom agents))
 (entry-point my-custom-agent))
```

### Agent Best Practices

1. **Loop Structure**: Always include a loop with sleep
2. **Resource Cleanup**: Handle termination gracefully
3. **Error Handling**: Use exception handlers
4. **Logging**: Use cog-logger for consistent logging
5. **State Sharing**: Use AtomSpace for shared state

### Testing

Run tests with:

```bash
make check TESTS=tests/services/opencog.scm
```

## API Reference

### Service Types

#### `opencog-orchestration-service-type`

Main service type for the orchestration daemon.

**Extensions**:
- `shepherd-root-service-type`: Provides Shepherd service

**Default Value**: `(opencog-orchestration-configuration)`

#### `opencog-agent-service-type`

Service type for individual agents (experimental).

### Configuration Records

#### `<opencog-orchestration-configuration>`

**Fields**:
- `atomspace` (package): AtomSpace package
- `cogserver` (package): CogServer package
- `attention` (package): Attention system package
- `agents` (list): List of agent configurations
- `port` (number): CogServer port
- `log-level` (string): Log verbosity level
- `data-directory` (string): Data storage directory

#### `<opencog-agent-configuration>`

**Fields**:
- `name` (symbol): Unique agent identifier
- `module` (list): Scheme module containing agent
- `entry-point` (procedure): Agent procedure (optional)
- `auto-start?` (boolean): Auto-start flag
- `dependencies` (list): Required agent names

### Orchestration API

**File**: `gnu/opencog/orchestration.scm`

#### `(make-orchestrator atomspace)`

Create new orchestrator instance.

**Returns**: `<orchestrator>` record

#### `(orchestrator-register-agent orchestrator name agent-proc dependencies)`

Register an agent with the orchestrator.

**Parameters**:
- `orchestrator`: Orchestrator instance
- `name`: Agent name (symbol)
- `agent-proc`: Agent procedure
- `dependencies`: List of dependency names

#### `(orchestrator-start-agent orchestrator name)`

Start a registered agent.

**Returns**: `#t` on success, `#f` if dependencies not met

#### `(orchestrator-stop-agent orchestrator name)`

Stop a running agent.

#### `(orchestrator-get-status orchestrator)`

Get status of all agents.

**Returns**: Association list of `(name . state)` pairs

#### `(orchestrator-run orchestrator)`

Run the orchestrator main loop (blocking).

## Troubleshooting

### Check Service Status

```bash
herd status opencog-orchestration
```

### View Logs

```bash
tail -f /var/log/opencog-orchestration.log
```

### Common Issues

**Service Won't Start**:
- Check networking service is running: `herd status networking`
- Verify port 17001 is available: `netstat -tuln | grep 17001`
- Check logs for errors

**Agent Not Starting**:
- Verify dependencies are running
- Check agent module is loadable
- Review agent configuration

**High CPU Usage**:
- Check agent sleep intervals
- Review agent loop implementation
- Monitor with `top` or `htop`

### Debug Mode

Enable debug logging:

```scheme
(opencog-orchestration-configuration
 (log-level "DEBUG"))
```

### Manual Testing

Start agents manually:

```scheme
(use-modules (gnu opencog orchestration))

(define orch (make-orchestrator atomspace))
(orchestrator-register-agent orch 'test-agent my-agent-proc '())
(orchestrator-start-agent orch 'test-agent)
(orchestrator-get-status orch)
```

## Performance Considerations

### Memory Usage

- Each agent runs in separate thread
- Shared AtomSpace reduces duplication
- Monitor with: `herd status opencog-orchestration`

### Concurrency

- Thread-safe operations via mutexes
- No data races in agent registry
- AtomSpace provides internal locking

### Scalability

- Tested with 5 concurrent agents
- Can scale to dozens of agents
- Limited by AtomSpace capacity and CPU cores

## Future Enhancements

1. **Dynamic Agent Loading**: Add/remove agents at runtime
2. **Message Passing**: Explicit inter-agent messaging
3. **Distributed Orchestration**: Multi-node coordination
4. **Performance Metrics**: Built-in monitoring dashboard
5. **Advanced Scheduling**: Priority-based agent scheduling
6. **Hot Reload**: Update agents without restart
