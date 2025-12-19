# Dan9 Testing & CI/CD Infrastructure - Quick Reference

## What Was Added

### 🔄 GitHub Actions Workflows
Located in `.github/workflows/`:

1. **`dan9-ci.yml`** - Continuous Integration
   - Triggers: Push/PR to main branches
   - Tests: Basic, extended modules, examples
   - Runtime: ~5-10 minutes

2. **`dan9-integration.yml`** - Integration Testing
   - Triggers: Push/PR, daily schedule, manual
   - Tests: E2E, stress, multi-daemon, integrated systems
   - Artifacts: Test logs and state (7-day retention)
   - Runtime: ~15-30 minutes

3. **`dan9-release.yml`** - Release Automation
   - Triggers: Version tags (v*.*.*), manual
   - Actions: Full test suite, create release archive, publish
   - Output: GitHub release with tarball

### 🧪 Test Suites
Located in `tests/`:

1. **`dan9.scm`** (existing) - Basic unit tests
2. **`dan9-e2e.scm`** (new) - End-to-end integration (13 test groups)
3. **`dan9-multi-daemon.scm`** (new) - Communication patterns (8 patterns)
4. **`dan9-stress.scm`** (new) - Stress testing (8 scenarios)

### 🛠️ Test Infrastructure
- **`scripts/run-dan9-tests.sh`** - Automated test runner with multiple modes
- **`tests/DAN9_TESTING.md`** - Comprehensive testing documentation

## Quick Start

### Run All Core Tests
```bash
./scripts/run-dan9-tests.sh
```

### Run Specific Test Categories
```bash
# Basic tests only
./scripts/run-dan9-tests.sh --basic-only

# Include stress tests
./scripts/run-dan9-tests.sh --with-stress

# Include examples
./scripts/run-dan9-tests.sh --with-examples

# Everything
./scripts/run-dan9-tests.sh --all

# Verbose output
./scripts/run-dan9-tests.sh --verbose
```

### Run Individual Test Files
```bash
# E2E integration tests
guile -L . tests/dan9-e2e.scm

# Multi-daemon communication
guile -L . tests/dan9-multi-daemon.scm

# Stress tests
guile -L . tests/dan9-stress.scm
```

## Test Coverage

### Core Daemon Functionality
- ✅ Daemon creation and lifecycle
- ✅ Registry operations
- ✅ Message passing
- ✅ Metrics collection

### Daemon Types
- ✅ Filesystem daemon
- ✅ Network daemon
- ✅ Process daemon
- ✅ Namespace daemon
- ✅ Persistence daemon
- ✅ Monitoring daemon
- ✅ Logging daemon
- ✅ Timer daemon

### Advanced Features
- ✅ Egregore orchestration (swarm, hierarchy, pipeline)
- ✅ Antikythera time-scale scheduling
- ✅ D9 address system
- ✅ Virtual devices

### Communication Patterns
- ✅ Point-to-point
- ✅ Broadcast
- ✅ Chain propagation
- ✅ Ring circulation
- ✅ Swarm coordination
- ✅ Hierarchical
- ✅ Pipeline
- ✅ Concurrent operations

### Performance & Stress
- ✅ High-volume daemon creation (100+)
- ✅ High-volume messaging (5000+)
- ✅ Rapid start/stop cycles
- ✅ Large egregore swarms (50+)
- ✅ Concurrent operations
- ✅ Memory stress
- ✅ Sustained load

## CI/CD Pipeline Flow

```
┌─────────────┐
│  Push/PR    │
└──────┬──────┘
       │
       ├────────────────┐
       │                │
       ▼                ▼
┌────────────┐  ┌──────────────┐
│ dan9-ci    │  │ dan9-        │
│            │  │ integration  │
│ • Basic    │  │ • E2E        │
│ • Modules  │  │ • Multi-D    │
│ • Examples │  │ • Stress     │
└────────────┘  └──────────────┘
                       │
                       ▼
                ┌──────────────┐
                │  Artifacts   │
                │  • Logs      │
                │  • State     │
                └──────────────┘

┌─────────────┐
│  Tag Push   │
│  (v*.*.*)   │
└──────┬──────┘
       │
       ▼
┌──────────────┐
│ dan9-release │
│              │
│ • Test All   │
│ • Archive    │
│ • Publish    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  GitHub      │
│  Release     │
└──────────────┘
```

## Test Statistics

- **Total Test Groups:** 29
- **Individual Tests:** 50+
- **Test Lines of Code:** ~3,600
- **Example Scripts:** 7
- **Communication Patterns:** 8
- **Stress Scenarios:** 8

## Documentation

- **Testing Guide:** `tests/DAN9_TESTING.md`
- **Quick Reference:** This file
- **Workflow Configs:** `.github/workflows/*.yml`

## Environment Variables

```bash
# Optional: Custom test directories
export DAN9_TEST_STATE_DIR=/custom/path/state
export DAN9_TEST_LOG_DIR=/custom/path/logs

# Automatically set by test runner
export GUILE_LOAD_PATH=/path/to/daeguix:$GUILE_LOAD_PATH
```

## Expected Performance

On typical CI systems:
- Basic tests: 5-10 seconds
- E2E tests: 30-60 seconds
- Multi-daemon: 30-60 seconds
- Stress tests: 60-120 seconds

## Next Steps

1. ✅ Infrastructure created
2. ✅ Tests implemented
3. ✅ Documentation written
4. ⏳ Run tests locally (requires Guile 3.0)
5. ⏳ Verify GitHub Actions workflows trigger correctly
6. ⏳ Monitor first CI run

## Troubleshooting

### Common Issues

**Guile not found:**
```bash
sudo apt-get install guile-3.0 guile-3.0-dev
```

**Module not found:**
```bash
export GUILE_LOAD_PATH=/path/to/daeguix:$GUILE_LOAD_PATH
```

**Tests timeout:**
Increase timeout in scripts or workflows.

**Race conditions:**
Tests include sleep delays for synchronization.

## Support

For detailed information, see:
- `tests/DAN9_TESTING.md` - Complete testing guide
- Workflow files for CI/CD specifics
- Test files for implementation details
