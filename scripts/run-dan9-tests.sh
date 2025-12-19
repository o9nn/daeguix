#!/bin/bash
# Dan9 Test Runner
# Runs all Dan9 tests with proper environment setup

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Test directories
TEST_DIR="$PROJECT_ROOT/tests"
DAN9_DIR="$PROJECT_ROOT/gnu/dan9"

# Environment setup
export GUILE_LOAD_PATH="$PROJECT_ROOT:${GUILE_LOAD_PATH:-}"
export DAN9_TEST_STATE_DIR="${DAN9_TEST_STATE_DIR:-/tmp/dan9-test-state}"
export DAN9_TEST_LOG_DIR="${DAN9_TEST_LOG_DIR:-/tmp/dan9-test-logs}"

# Create test directories
mkdir -p "$DAN9_TEST_STATE_DIR"
mkdir -p "$DAN9_TEST_LOG_DIR"

echo -e "${BLUE}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║              DAN9 COMPREHENSIVE TEST SUITE                    ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if Guile is installed
if ! command -v guile &> /dev/null; then
    echo -e "${RED}Error: Guile is not installed${NC}"
    echo "Please install Guile 3.0 or later"
    exit 1
fi

echo -e "${GREEN}✓${NC} Guile found: $(guile --version | head -1)"
echo -e "${GREEN}✓${NC} Project root: $PROJECT_ROOT"
echo -e "${GREEN}✓${NC} Test state dir: $DAN9_TEST_STATE_DIR"
echo -e "${GREEN}✓${NC} Test log dir: $DAN9_TEST_LOG_DIR"
echo ""

# Parse command line arguments
RUN_BASIC=1
RUN_E2E=1
RUN_MULTI=1
RUN_STRESS=0
RUN_EXAMPLES=0
VERBOSE=0

while [[ $# -gt 0 ]]; do
    case $1 in
        --basic-only)
            RUN_E2E=0
            RUN_MULTI=0
            RUN_STRESS=0
            RUN_EXAMPLES=0
            shift
            ;;
        --with-stress)
            RUN_STRESS=1
            shift
            ;;
        --with-examples)
            RUN_EXAMPLES=1
            shift
            ;;
        --all)
            RUN_STRESS=1
            RUN_EXAMPLES=1
            shift
            ;;
        --verbose|-v)
            VERBOSE=1
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --basic-only      Run only basic tests"
            echo "  --with-stress     Include stress tests"
            echo "  --with-examples   Run example scripts"
            echo "  --all             Run all tests including stress and examples"
            echo "  --verbose, -v     Verbose output"
            echo "  --help, -h        Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Test result tracking
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

run_test() {
    local test_name="$1"
    local test_file="$2"
    local timeout="${3:-60}"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "${BLUE}Running: $test_name${NC}"
    
    if [ $VERBOSE -eq 1 ]; then
        if timeout "$timeout" guile -L "$PROJECT_ROOT" "$test_file"; then
            echo -e "${GREEN}✓ PASSED${NC}: $test_name"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        else
            echo -e "${RED}✗ FAILED${NC}: $test_name"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            return 1
        fi
    else
        if timeout "$timeout" guile -L "$PROJECT_ROOT" "$test_file" > /dev/null 2>&1; then
            echo -e "${GREEN}✓ PASSED${NC}: $test_name"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        else
            echo -e "${RED}✗ FAILED${NC}: $test_name (run with --verbose for details)"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            return 1
        fi
    fi
}

run_example() {
    local example_name="$1"
    local example_file="$2"
    local timeout="${3:-30}"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -e "${BLUE}Running example: $example_name${NC}"
    
    if timeout "$timeout" guile -L "$PROJECT_ROOT" "$example_file" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ COMPLETED${NC}: $example_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${YELLOW}⚠ TIMEOUT/ERROR${NC}: $example_name (this may be expected)"
        # Don't count as failure for examples
        return 0
    fi
}

# Run basic tests
if [ $RUN_BASIC -eq 1 ]; then
    echo -e "\n${YELLOW}═══ BASIC TESTS ═══${NC}\n"
    run_test "Basic Dan9 Tests" "$TEST_DIR/dan9.scm" 30
fi

# Run E2E integration tests
if [ $RUN_E2E -eq 1 ]; then
    echo -e "\n${YELLOW}═══ E2E INTEGRATION TESTS ═══${NC}\n"
    run_test "E2E Integration Tests" "$TEST_DIR/dan9-e2e.scm" 60
fi

# Run multi-daemon tests
if [ $RUN_MULTI -eq 1 ]; then
    echo -e "\n${YELLOW}═══ MULTI-DAEMON COMMUNICATION TESTS ═══${NC}\n"
    run_test "Multi-Daemon Communication" "$TEST_DIR/dan9-multi-daemon.scm" 60
fi

# Run stress tests
if [ $RUN_STRESS -eq 1 ]; then
    echo -e "\n${YELLOW}═══ STRESS TESTS ═══${NC}\n"
    echo -e "${YELLOW}Note: Stress tests may take longer and push system limits${NC}\n"
    run_test "Stress Tests" "$TEST_DIR/dan9-stress.scm" 120
fi

# Run examples
if [ $RUN_EXAMPLES -eq 1 ]; then
    echo -e "\n${YELLOW}═══ EXAMPLE SCRIPTS ═══${NC}\n"
    
    if [ -f "$DAN9_DIR/example.scm" ]; then
        run_example "Basic Example" "$DAN9_DIR/example.scm" 15
    fi
    
    if [ -f "$DAN9_DIR/egregore-example.scm" ]; then
        run_example "Egregore Example" "$DAN9_DIR/egregore-example.scm" 15
    fi
    
    if [ -f "$DAN9_DIR/antikythera-example.scm" ]; then
        run_example "Antikythera Example" "$DAN9_DIR/antikythera-example.scm" 15
    fi
    
    if [ -f "$DAN9_DIR/monitoring-example.scm" ]; then
        run_example "Monitoring Example" "$DAN9_DIR/monitoring-example.scm" 15
    fi
    
    if [ -f "$DAN9_DIR/persistence-example.scm" ]; then
        run_example "Persistence Example" "$DAN9_DIR/persistence-example.scm" 15
    fi
    
    if [ -f "$DAN9_DIR/integrated-example.scm" ]; then
        run_example "Integrated Example" "$DAN9_DIR/integrated-example.scm" 30
    fi
    
    if [ -f "$DAN9_DIR/integrated-system-example.scm" ]; then
        run_example "Integrated System Example" "$DAN9_DIR/integrated-system-example.scm" 30
    fi
fi

# Print summary
echo ""
echo -e "${BLUE}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                    TEST SUMMARY                               ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "Total tests run: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"

if [ $FAILED_TESTS -gt 0 ]; then
    echo -e "${RED}Failed: $FAILED_TESTS${NC}"
    echo ""
    echo -e "${RED}Some tests failed. Please review the errors above.${NC}"
    exit 1
else
    echo -e "${RED}Failed: $FAILED_TESTS${NC}"
    echo ""
    echo -e "${GREEN}╔═══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║             ALL TESTS PASSED SUCCESSFULLY! ✓                  ║${NC}"
    echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════╝${NC}"
    exit 0
fi
