# Dan9 GitHub Actions Workflows

This directory contains the CI/CD workflows for the Dan9 daemon-centric system.

## Workflows

### 1. Dan9 CI (`dan9-ci.yml`)

**Purpose:** Continuous integration testing for basic functionality

**When it runs:**
- On push to `main`, `master`, `develop`, or `copilot/**` branches
- On pull requests to `main`, `master`, or `develop`
- When Dan9 files are modified
- Can be manually triggered

**What it tests:**
- **Basic Dan9 Tests:** Core daemon functionality
- **Extended Module Tests:** All Dan9 modules load correctly
- **Example Scripts:** Example scripts execute without errors

**Duration:** ~5-10 minutes

### 2. Dan9 Integration Tests (`dan9-integration.yml`)

**Purpose:** Comprehensive end-to-end integration and stress testing

**When it runs:**
- On push to main branches (when Dan9 files change)
- On pull requests
- Daily at 2 AM UTC (scheduled)
- Can be manually triggered

**What it tests:**
- **E2E Integration Tests:** Complete system integration
- **Stress Tests:** Performance under load (continues on error)
- **Integration Examples:** Complex integrated system scenarios
- **Multi-Daemon Tests:** Communication patterns between daemons

**Artifacts Generated:**
- Test logs (7-day retention)
- Test state (7-day retention)

**Duration:** ~15-30 minutes

### 3. Dan9 Release (`dan9-release.yml`)

**Purpose:** Automated release process

**When it runs:**
- On push of version tags (e.g., `v1.0.0`, `v0.1.0`)
- Can be manually triggered with version input

**What it does:**
1. **Build and Test:** Runs full test suite
2. **Verify Modules:** Ensures all modules load
3. **Create Archive:** Packages Dan9 as a tarball
4. **Generate Release Notes:** Auto-generated release documentation
5. **Publish Release:** Creates GitHub release with archive

**Output:** GitHub release with `dan9-VERSION.tar.gz`

**Duration:** ~10-15 minutes

## Workflow Dependencies

```
dan9-ci.yml
  ├─ Basic tests
  ├─ Module loading
  └─ Examples

dan9-integration.yml
  ├─ E2E tests
  ├─ Stress tests
  ├─ Integration examples
  └─ Multi-daemon tests

dan9-release.yml
  └─ Depends on: dan9-ci + dan9-integration
     (Must pass before release)
```

## Required Secrets

None - all workflows use default `GITHUB_TOKEN`

## Required Permissions

- **Contents:** read (checkout code)
- **Actions:** read (workflow status)
- **Packages:** write (for releases)

## Status Badges

Add to your README.md:

```markdown
[![Dan9 CI](https://github.com/o9nn/daeguix/workflows/Dan9%20CI/badge.svg)](https://github.com/o9nn/daeguix/actions/workflows/dan9-ci.yml)

[![Dan9 Integration](https://github.com/o9nn/daeguix/workflows/Dan9%20Integration%20Tests/badge.svg)](https://github.com/o9nn/daeguix/actions/workflows/dan9-integration.yml)
```

## Running Locally

To run the same tests locally:

```bash
# Install Guile 3.0
sudo apt-get install guile-3.0 guile-3.0-dev

# Run tests
./scripts/run-dan9-tests.sh

# Or individual test suites
guile -L . tests/dan9.scm
guile -L . tests/dan9-e2e.scm
guile -L . tests/dan9-multi-daemon.scm
guile -L . tests/dan9-stress.scm
```

## Triggering Workflows Manually

### Via GitHub UI
1. Go to Actions tab
2. Select workflow
3. Click "Run workflow"
4. Choose branch (and version for release)
5. Click "Run workflow"

### Via GitHub CLI
```bash
# Trigger CI
gh workflow run dan9-ci.yml

# Trigger integration tests
gh workflow run dan9-integration.yml

# Trigger release
gh workflow run dan9-release.yml -f version=v0.1.0
```

## Monitoring Workflow Runs

### View Status
```bash
# List recent runs
gh run list --workflow=dan9-ci.yml

# Watch a specific run
gh run watch <run-id>

# View logs
gh run view <run-id> --log
```

### Download Artifacts
```bash
# List artifacts
gh run view <run-id>

# Download specific artifact
gh run download <run-id> -n dan9-test-logs
```

## Workflow Files Explained

### Common Structure

```yaml
name: Workflow Name

on:
  push:
    branches: [ ... ]
    paths: [ ... ]  # Only run if these files change
  pull_request:
    branches: [ ... ]
  schedule:
    - cron: '...'   # Scheduled runs
  workflow_dispatch:  # Manual trigger

jobs:
  job-name:
    name: Display Name
    runs-on: ubuntu-latest
    timeout-minutes: 30
    
    steps:
    - name: Checkout
      uses: actions/checkout@v4
      
    - name: Install dependencies
      run: |
        sudo apt-get update
        sudo apt-get install -y guile-3.0 guile-3.0-dev
        
    - name: Run tests
      run: |
        guile -L . tests/dan9.scm
```

## Customization

### Change Test Schedule

Edit `dan9-integration.yml`:
```yaml
schedule:
  - cron: '0 2 * * *'  # Daily at 2 AM UTC
```

Common cron expressions:
- `'0 */6 * * *'` - Every 6 hours
- `'0 0 * * 0'` - Weekly on Sunday
- `'0 0 1 * *'` - Monthly on the 1st

### Add More Tests

1. Create test file in `tests/`
2. Add to `scripts/run-dan9-tests.sh`
3. Add step to appropriate workflow

### Change Timeout

Edit job `timeout-minutes`:
```yaml
jobs:
  test:
    timeout-minutes: 60  # Increase if needed
```

## Troubleshooting

### Workflow Failed

1. Check logs in Actions tab
2. Look for specific test failures
3. Run failing test locally:
   ```bash
   guile -L . tests/failing-test.scm
   ```

### Artifact Not Found

Artifacts are only created if:
- Job runs to completion (even with errors)
- Artifact upload step executes
- Artifacts expire after retention period (7 days)

### Workflow Not Triggering

Check:
- File paths in `on.push.paths` match changed files
- Branch names match `on.push.branches`
- Workflow file has valid YAML syntax

## Best Practices

1. **Keep workflows fast:** Use caching, parallel jobs
2. **Fail fast:** Run quick tests first
3. **Use artifacts:** Save logs and test results
4. **Set timeouts:** Prevent hanging jobs
5. **Document changes:** Update this README when modifying workflows

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Workflow Syntax](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)
- [Using Artifacts](https://docs.github.com/en/actions/guides/storing-workflow-data-as-artifacts)
- [Scheduled Events](https://docs.github.com/en/actions/reference/events-that-trigger-workflows#scheduled-events)
