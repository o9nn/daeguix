# OpenCog Autonomous Multi-Agent Orchestration - Quick Start Guide

## Introduction

This guide will help you get started with the OpenCog autonomous multi-agent orchestration workbench on GNU Guix.

## Prerequisites

- GNU Guix system installed
- Basic understanding of Scheme
- Root or sudo access for system configuration

## Installation

### Step 1: Add OpenCog Service to Your System

Edit your system configuration file (e.g., `/etc/config.scm`):

```scheme
(use-modules (gnu)
             (gnu services opencog))

(use-service-modules desktop networking ssh)

(operating-system
  ;; ... other configuration ...
  
  (services
   (append
    ;; Add OpenCog orchestration service
    (list (service opencog-orchestration-service-type))
    
    ;; ... other services ...
    %base-services)))
```

### Step 2: Reconfigure Your System

```bash
sudo guix system reconfigure /etc/config.scm
```

This will:
1. Download and build OpenCog packages (atomspace, cogserver, attention)
2. Set up the orchestration service
3. Configure Shepherd to manage the daemon

### Step 3: Start the Service

```bash
sudo herd start opencog-orchestration
```

### Step 4: Verify It's Running

```bash
sudo herd status opencog-orchestration
```

You should see output like:
```
Status of opencog-orchestration:
  It is started.
  Running value is ...
```

## Basic Configuration

### Configure Agents

To add specific agents to your orchestration:

```scheme
(service opencog-orchestration-service-type
         (opencog-orchestration-configuration
          (port 17001)
          (log-level "INFO")
          (agents
           (list
            ;; Reasoning agent
            (opencog-agent-configuration
             (name 'reasoning-agent)
             (module '(gnu opencog agents))
             (auto-start? #t))
            
            ;; Learning agent (depends on reasoning)
            (opencog-agent-configuration
             (name 'learning-agent)
             (module '(gnu opencog agents))
             (dependencies '(reasoning-agent))
             (auto-start? #t))))))
```

### Change Default Port

```scheme
(opencog-orchestration-configuration
 (port 18001))  ; Use port 18001 instead of default 17001
```

### Adjust Logging Level

```scheme
(opencog-orchestration-configuration
 (log-level "DEBUG"))  ; Options: "DEBUG", "INFO", "WARN", "ERROR"
```

## Monitoring

### Check Service Status

```bash
sudo herd status opencog-orchestration
```

### View Logs

```bash
sudo tail -f /var/log/opencog-orchestration.log
```

### See All Services

```bash
sudo herd status
```

## Interacting with OpenCog

### Using CogServer

Connect to the running CogServer:

```bash
telnet localhost 17001
```

Or using netcat:

```bash
nc localhost 17001
```

### From Scheme REPL

```bash
guile
```

Then in the REPL:

```scheme
(use-modules (opencog))
(use-modules (opencog exec))

;; Connect to running CogServer
;; (Implementation depends on your setup)
```

## Example: Complete System Configuration

Here's a complete example for a dedicated OpenCog workbench:

```scheme
(use-modules (gnu)
             (gnu services opencog)
             (gnu services networking)
             (gnu services ssh))

(use-service-modules desktop networking ssh)
(use-package-modules screen)

(operating-system
  (host-name "opencog-box")
  (timezone "UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))))

  (file-systems (cons (file-system
                        (device "/dev/sda1")
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "opencog")
                (group "users")
                (supplementary-groups '("wheel"))
                (home-directory "/home/opencog"))
               %base-user-accounts))

  (packages (cons screen %base-packages))

  (services
   (append
    (list
     ;; OpenCog with all agents
     (service opencog-orchestration-service-type
              (opencog-orchestration-configuration
               (port 17001)
               (log-level "INFO")
               (agents
                (list
                 (opencog-agent-configuration
                  (name 'reasoning-agent)
                  (module '(gnu opencog agents)))
                 (opencog-agent-configuration
                  (name 'attention-agent)
                  (module '(gnu opencog agents)))
                 (opencog-agent-configuration
                  (name 'learning-agent)
                  (module '(gnu opencog agents))
                  (dependencies '(reasoning-agent)))
                 (opencog-agent-configuration
                  (name 'planning-agent)
                  (module '(gnu opencog agents))
                  (dependencies '(reasoning-agent attention-agent)))
                 (opencog-agent-configuration
                  (name 'communication-agent)
                  (module '(gnu opencog agents)))))))
     
     ;; SSH for remote access
     (service openssh-service-type))
    
    %base-services)))
```

Save this as `opencog-workbench.scm` and install:

```bash
sudo guix system reconfigure opencog-workbench.scm
```

## Creating Custom Agents

### Step 1: Create Agent Module

Create file `/home/opencog/my-agent.scm`:

```scheme
(define-module (my-agent)
  #:export (my-custom-agent))

(use-modules (opencog)
             (opencog exec)
             (ice-9 format))

(define (my-custom-agent atomspace)
  "A simple custom agent that runs every 10 seconds."
  (let loop ((count 0))
    (format #t "My custom agent running, iteration ~a~%" count)
    
    ;; Do something with the atomspace
    ;; (cog-execute! ...)
    
    (sleep 10)
    (loop (+ count 1))))
```

### Step 2: Register in System Configuration

```scheme
(opencog-agent-configuration
 (name 'my-custom-agent)
 (module '(my-agent))
 (entry-point my-custom-agent)
 (auto-start? #t))
```

### Step 3: Update Module Path

Make sure Guile can find your module:

```bash
export GUILE_LOAD_PATH="/home/opencog:$GUILE_LOAD_PATH"
```

Or add to service configuration.

## Troubleshooting

### Service Won't Start

**Check dependencies**:
```bash
sudo herd status networking
```

**Check configuration syntax**:
```bash
guix system build /etc/config.scm
```

**View detailed errors**:
```bash
sudo journalctl -u shepherd -f
```

### Agent Not Running

**Check agent registration**:
Review logs at `/var/log/opencog-orchestration.log`

**Verify dependencies**:
Ensure dependency agents are running before dependent agents

**Test agent manually**:
```bash
guile
(load "my-agent.scm")
(my-custom-agent #f)  ; Test without atomspace
```

### Port Already in Use

**Check what's using the port**:
```bash
sudo netstat -tuln | grep 17001
```

**Change to different port**:
```scheme
(opencog-orchestration-configuration
 (port 17002))
```

### High Resource Usage

**Monitor processes**:
```bash
top
# Look for guile processes
```

**Increase sleep intervals in agents**:
Modify agent code to sleep longer between iterations

**Reduce number of agents**:
Comment out unnecessary agents in configuration

## Next Steps

1. **Read Full Documentation**: See `TECHNICAL.md` for detailed architecture
2. **Explore AtomSpace**: Learn about knowledge representation
3. **Develop Custom Agents**: Build agents for your specific use case
4. **Join Community**: Connect with other OpenCog users
5. **Contribute**: Submit improvements to the orchestration framework

## Resources

- OpenCog Website: https://opencog.org/
- AtomSpace Documentation: https://wiki.opencog.org/w/AtomSpace
- GNU Guix Manual: https://guix.gnu.org/manual/
- Shepherd Manual: https://www.gnu.org/software/shepherd/manual/

## Getting Help

- Check logs: `/var/log/opencog-orchestration.log`
- Ask on Guix mailing list: help-guix@gnu.org
- OpenCog IRC: #opencog on Libera.Chat
- File issues on the repository

---

**Note**: This is a daemon-based implementation focused on orchestration. For heavy computational AI workloads, you may need to tune performance parameters and resource limits.
