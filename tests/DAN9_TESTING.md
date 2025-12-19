# Dan9 Testing Guide

This document describes the comprehensive testing infrastructure for the Dan9 daemon-centric system.

## Overview

The Dan9 testing suite includes:
- **Basic unit tests** - Core daemon functionality
- **E2E integration tests** - Full system integration testing
- **Multi-daemon communication tests** - Complex daemon interaction patterns
- **Stress tests** - Performance and load testing
- **Example scripts** - Demonstration and validation of features

## Test Structure

```
tests/
├── dan9.scm              # Basic unit tests
├── dan9-e2e.scm          # End-to-end integration tests
├── dan9-multi-daemon.scm # Multi-daemon communication tests
└── dan9-stress.scm       # Stress and performance tests

scripts/
└── run-dan9-tests.sh     # Test runner script

.github/workflows/
├── dan9-ci.yml           # Basic CI workflow
├── dan9-integration.yml  # Integration testing workflow
└── dan9-release.yml      # Release workflow
```

## Running Tests

### Quick Start

Run all core tests (basic, e2e, multi-daemon):
```bash
./scripts/run-dan9-tests.sh
```

### Test Options

Run only basic tests:
```bash
./scripts/run-dan9-tests.sh --basic-only
```

Run all tests including stress tests:
```bash
./scripts/run-dan9-tests.sh --with-stress
```

Run all tests including examples:
```bash
./scripts/run-dan9-tests.sh --with-examples
```

Run everything:
```bash
./scripts/run-dan9-tests.sh --all
```

Enable verbose output:
```bash
./scripts/run-dan9-tests.sh --verbose
```

### Manual Test Execution

Run individual test files:
```bash
# Basic tests
guile -L . tests/dan9.scm

# E2E integration tests
guile -L . tests/dan9-e2e.scm

# Multi-daemon communication tests
guile -L . tests/dan9-multi-daemon.scm

# Stress tests
guile -L . tests/dan9-stress.scm
```

## Test Categories

### 1. Basic Unit Tests (`tests/dan9.scm`)

Tests core daemon functionality:
- Daemon creation and initialization
- Registry operations
- Metrics collection
- Status reporting
- Basic daemon types (filesystem, network, process, namespace)

**Run time:** ~5-10 seconds

### 2. E2E Integration Tests (`tests/dan9-e2e.scm`)

Comprehensive integration testing:
- Multi-daemon communication
- Daemon lifecycle management (start/stop/restart)
- Persistence and recovery
- Monitoring integration
- Logging integration
- Timer and scheduling
- Namespace bindings
- Egregore orchestration (swarm, hierarchy, pipeline)
- Antikythera time-scale scheduling
- Full system integration
- Message passing under load
- Error handling and recovery
- Performance and resource management

**Run time:** ~30-60 seconds

**Test groups:**
- `multi-daemon-communication` - Inter-daemon messaging
- `daemon-lifecycle` - Start, stop, restart operations
- `persistence-recovery` - State persistence
- `monitoring-integration` - Metrics collection
- `logging-integration` - Centralized logging
- `timer-scheduling` - Scheduled tasks
- `namespace-bindings` - Namespace operations
- `egregore-orchestration` - Daemon coordination patterns
- `antikythera-scheduling` - Time-scaled events
- `full-system-integration` - Complete system tests
- `message-passing-load` - High-volume messaging
- `error-handling` - Error scenarios
- `performance` - Resource management

### 3. Multi-Daemon Communication Tests (`tests/dan9-multi-daemon.scm`)

Tests complex communication patterns:
- **Point-to-point** - Direct daemon-to-daemon messaging
- **Broadcast** - One-to-many communication
- **Chain** - Sequential message propagation
- **Ring** - Circular message passing
- **Swarm coordination** - Egregore-managed swarms
- **Hierarchical** - Multi-level communication
- **Pipeline** - Sequential processing
- **Concurrent operations** - Multiple groups simultaneously

**Run time:** ~30-60 seconds

### 4. Stress Tests (`tests/dan9-stress.scm`)

Performance and load testing:
- High-volume daemon creation (100+ daemons)
- High-volume message passing (1000+ messages)
- Rapid start/stop cycles (20+ cycles)
- Large egregore swarms (50+ daemons)
- Concurrent egregore operations (10+ egregores)
- Memory stress (1000+ metrics)
- Message queue stress (5000+ messages)
- Sustained load testing

**Run time:** ~60-120 seconds

**Note:** Stress tests intentionally push system limits and may experience some performance degradation. This is expected behavior.

## CI/CD Workflows

### CI Workflow (`.github/workflows/dan9-ci.yml`)

Runs on every push and pull request:
- Basic Dan9 tests
- Extended module tests
- Example script validation

**Triggers:**
- Push to main/master/develop/copilot/** branches
- Pull requests to main/master/develop
- Manual workflow dispatch

### Integration Workflow (`.github/workflows/dan9-integration.yml`)

Comprehensive integration testing:
- E2E integration tests
- Stress tests (continue-on-error)
- Integrated system examples
- Multi-daemon communication tests

**Triggers:**
- Push to main/master/develop/copilot/** branches
- Pull requests to main/master/develop
- Daily at 2 AM UTC (scheduled)
- Manual workflow dispatch

**Artifacts:**
- Test logs (7-day retention)
- Test state (7-day retention)

### Release Workflow (`.github/workflows/dan9-release.yml`)

Automated release process:
- Build and test verification
- Module loading verification
- Release archive creation
- GitHub release creation

**Triggers:**
- Push of version tags (v*.*.*)
- Manual workflow dispatch with version input

## Environment Variables

### Test Configuration

```bash
# Test state directory (default: /tmp/dan9-test-state)
export DAN9_TEST_STATE_DIR=/path/to/state

# Test log directory (default: /tmp/dan9-test-logs)
export DAN9_TEST_LOG_DIR=/path/to/logs

# Guile load path (automatically set by test runner)
export GUILE_LOAD_PATH=/path/to/daeguix:$GUILE_LOAD_PATH
```

## Writing New Tests

### Test Structure

Use SRFI-64 testing framework:

```scheme
(define-module (test-my-feature)
  #:use-module (gnu dan9 daemons)
  #:use-module (srfi srfi-64))

(test-begin "my-feature")

(test-group "feature-group"
  (test-assert "test description"
    ;; Test code
    #t))

(test-end "my-feature")
```

### Test Helpers

Common patterns for daemon testing:

```scheme
;; Wait for daemon to start
(define (wait-for-daemon daemon max-wait)
  (let loop ((remaining max-wait))
    (if (or (<= remaining 0)
            (eq? 'running (daemon-state daemon)))
        (eq? 'running (daemon-state daemon))
        (begin
          (usleep 100000)
          (loop (- remaining 0.1))))))

;; Cleanup daemon after test
(define (cleanup-daemon daemon)
  (when (and daemon (daemon? daemon))
    (daemon-stop daemon)
    (usleep 100000)))
```

### Test Guidelines

1. **Isolation** - Tests should not depend on each other
2. **Cleanup** - Always stop daemons and clean up resources
3. **Timeouts** - Use reasonable timeouts for async operations
4. **Idempotency** - Tests should be repeatable
5. **Clear names** - Use descriptive test names
6. **Documentation** - Comment complex test scenarios

## Continuous Integration

### GitHub Actions

All workflows use GitHub Actions with:
- Ubuntu latest runner
- Guile 3.0 installation
- Proper load path configuration
- Artifact upload for debugging

### Status Badges

Add to README.md:

```markdown
[![Dan9 CI](https://github.com/o9nn/daeguix/workflows/Dan9%20CI/badge.svg)](https://github.com/o9nn/daeguix/actions)
[![Dan9 Integration](https://github.com/o9nn/daeguix/workflows/Dan9%20Integration%20Tests/badge.svg)](https://github.com/o9nn/daeguix/actions)
```

## Troubleshooting

### Common Issues

**Guile not found:**
```bash
# Install Guile 3.0
sudo apt-get install guile-3.0 guile-3.0-dev
```

**Module not found:**
```bash
# Ensure GUILE_LOAD_PATH includes project root
export GUILE_LOAD_PATH=/path/to/daeguix:$GUILE_LOAD_PATH
```

**Tests timeout:**
- Increase timeout in workflow or script
- Check for deadlocks in daemon loops
- Ensure daemons are properly stopped

**Race conditions:**
- Add sleep delays between operations
- Use wait-for-daemon helper
- Check thread synchronization

### Debug Mode

Run with verbose output:
```bash
./scripts/run-dan9-tests.sh --verbose
```

Or manually:
```bash
guile -L . --debug tests/dan9-e2e.scm
```

## Performance Benchmarks

Expected performance metrics (may vary by system):

- **Daemon creation:** < 1ms per daemon
- **Message sending:** < 0.1ms per message
- **Message receiving:** < 1ms per message (with processing)
- **Start/stop cycle:** < 100ms
- **Egregore coordination:** < 10ms overhead per daemon

## Contributing

When adding new tests:

1. Add test file to `tests/` directory
2. Update `run-dan9-tests.sh` to include new test
3. Add test to appropriate CI workflow
4. Update this documentation
5. Ensure tests pass locally before submitting PR

## Resources

- [SRFI-64 Testing Documentation](https://srfi.schemers.org/srfi-64/srfi-64.html)
- [Guile Reference Manual](https://www.gnu.org/software/guile/manual/)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)

## License

All test code is licensed under GNU General Public License v3 or later (GPL-3.0+).
