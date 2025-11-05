;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 OpenCog Orchestration Contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services opencog)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages opencog)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (opencog-orchestration-configuration
            opencog-orchestration-configuration?
            opencog-orchestration-configuration-atomspace
            opencog-orchestration-configuration-cogserver
            opencog-orchestration-configuration-attention
            opencog-orchestration-configuration-agents
            opencog-orchestration-configuration-port
            opencog-orchestration-configuration-log-level
            opencog-orchestration-configuration-data-directory

            opencog-agent-configuration
            opencog-agent-configuration?
            opencog-agent-configuration-name
            opencog-agent-configuration-module
            opencog-agent-configuration-entry-point
            opencog-agent-configuration-auto-start?
            opencog-agent-configuration-dependencies

            opencog-orchestration-service-type
            opencog-agent-service-type))

;;;
;;; OpenCog Orchestration Configuration
;;;

(define-record-type* <opencog-orchestration-configuration>
  opencog-orchestration-configuration make-opencog-orchestration-configuration
  opencog-orchestration-configuration?
  (atomspace opencog-orchestration-configuration-atomspace
             (default atomspace))
  (cogserver opencog-orchestration-configuration-cogserver
             (default cogserver))
  (attention opencog-orchestration-configuration-attention
             (default attention))
  (agents opencog-orchestration-configuration-agents
          (default '()))
  (port opencog-orchestration-configuration-port
        (default 17001))
  (log-level opencog-orchestration-configuration-log-level
             (default "INFO"))
  (data-directory opencog-orchestration-configuration-data-directory
                  (default "/var/lib/opencog")))

(define-record-type* <opencog-agent-configuration>
  opencog-agent-configuration make-opencog-agent-configuration
  opencog-agent-configuration?
  (name opencog-agent-configuration-name)
  (module opencog-agent-configuration-module)
  (entry-point opencog-agent-configuration-entry-point
               (default #f))
  (auto-start? opencog-agent-configuration-auto-start?
               (default #t))
  (dependencies opencog-agent-configuration-dependencies
                (default '())))

;;;
;;; Orchestration Daemon
;;;

(define (opencog-orchestration-shepherd-service config)
  "Return a <shepherd-service> for OpenCog orchestration with CONFIG."
  (match-record config <opencog-orchestration-configuration>
    (atomspace cogserver attention port log-level data-directory agents)
    (let ((orchestrator-script
           (program-file "opencog-orchestrator"
             #~(begin
                 (use-modules (opencog)
                              (opencog exec)
                              (opencog logger)
                              (opencog cogserver)
                              (opencog attention)
                              (gnu opencog orchestration)
                              (gnu opencog agents)
                              (gnu opencog namespaces)
                              (gnu opencog distributed)
                              (gnu opencog daemons)
                              (gnu opencog learning)
                              (ice-9 threads)
                              (ice-9 match))

                 ;; Initialize logging
                 (cog-logger-set-level! #$log-level)
                 (cog-logger-info "Starting OpenCog Orchestration Service with Plan9/Inferno features")

                 ;; Create data directory if needed
                 (unless (file-exists? #$data-directory)
                   (mkdir-p #$data-directory))

                 ;; Initialize AtomSpace
                 (define atomspace (cog-atomspace))
                 (cog-logger-info "AtomSpace initialized")

                 ;; Start CogServer
                 (define cogserver-instance
                   (start-cogserver #:port #$port))
                 (cog-logger-info
                  (format #f "CogServer started on port ~a" #$port))

                 ;; Create orchestrator with integrated features
                 (define orchestrator (make-orchestrator atomspace))
                 (cog-logger-info "Orchestrator created with namespace and distributed coordination")

                 ;; Start system daemons
                 (define resource-daemon
                   (make-resource-monitor-daemon #:interval 10))
                 (orchestrator-add-daemon orchestrator resource-daemon)
                 (cog-logger-info "Resource monitor daemon started")

                 (define atomspace-sync-daemon
                   (make-atomspace-sync-daemon #:sync-interval 30))
                 (orchestrator-add-daemon orchestrator atomspace-sync-daemon)
                 (cog-logger-info "AtomSpace sync daemon started")

                 (define attention-daemon
                   (make-attention-allocation-daemon #:allocation-interval 5))
                 (orchestrator-add-daemon orchestrator attention-daemon)
                 (cog-logger-info "Attention allocation daemon started")

                 (define health-daemon
                   (make-health-monitor-daemon #:check-interval 15))
                 (orchestrator-add-daemon orchestrator health-daemon)
                 (cog-logger-info "Health monitor daemon started")

                 ;; Start 9P service for namespace access
                 (9p-service-start (orchestrator-9p-service orchestrator))
                 (cog-logger-info "9P-style namespace service started")

                 ;; Initialize distributed coordination
                 (coordinator-register-node (orchestrator-coordinator orchestrator)
                                           'local-node
                                           "localhost"
                                           '(reasoning learning planning))
                 (cog-logger-info "Local node registered in distributed coordinator")

                 ;; Agent registry and coordination using orchestrator
                 (define (register-agent name agent-proc dependencies)
                   (orchestrator-register-agent orchestrator name agent-proc dependencies)
                   (cog-logger-info
                    (format #f "Agent registered: ~a" name)))

                 (define (start-agent name)
                   (if (orchestrator-start-agent orchestrator name)
                       (cog-logger-info (format #f "Agent started: ~a" name))
                       (cog-logger-warn (format #f "Failed to start agent: ~a" name))))

                 (define (stop-agent name)
                   (when (orchestrator-stop-agent orchestrator name)
                     (cog-logger-info (format #f "Agent stopped: ~a" name))))

                 (define (get-agent-status name)
                   (let ((statuses (orchestrator-get-status orchestrator)))
                     (assoc-ref statuses name)))

                 ;; Orchestration control loop
                 (define (orchestration-loop)
                   (let loop ()
                     ;; Monitor agent states using orchestrator
                     (let ((statuses (orchestrator-get-status orchestrator)))
                       (for-each
                        (lambda (status-pair)
                          (when (eq? (cdr status-pair) 'error)
                            (cog-logger-error
                             (format #f "Agent in error state: ~a" (car status-pair)))))
                        statuses))
                     
                     ;; Check daemon health
                     (for-each
                      (lambda (daemon)
                        (let ((metrics (daemon-get-metrics daemon)))
                          (when (null? metrics)
                            (cog-logger-warn
                             (format #f "Daemon ~a has no metrics" (daemon-name daemon))))))
                      (orchestrator-daemons orchestrator))
                     
                     ;; Sleep and continue
                     (sleep 5)
                     (loop)))

                 ;; Start orchestration monitoring thread
                 (call-with-new-thread orchestration-loop)
                 (cog-logger-info "Orchestration monitoring thread started")

                 ;; Register configured agents
                 #$@(map (lambda (agent)
                          #~(register-agent
                             '#$(opencog-agent-configuration-name agent)
                             (lambda (as)
                               (cog-logger-info
                                (format #f "Running agent: ~a"
                                        '#$(opencog-agent-configuration-name agent)))
                               ;; Placeholder agent implementation.
                               ;; In production, this should be replaced with actual
                               ;; agent logic loaded from the configured module.
                               ;; This basic loop keeps the agent thread alive.
                               (let agent-loop ()
                                 (sleep 10)
                                 (agent-loop)))
                             '#$(opencog-agent-configuration-dependencies agent)))
                        agents)

                 ;; Auto-start agents
                 #$@(map (lambda (agent)
                          (if (opencog-agent-configuration-auto-start? agent)
                              #~(start-agent
                                 '#$(opencog-agent-configuration-name agent))
                              #~#f))
                        agents)

                 ;; Keep the main process running
                 (cog-logger-info "Orchestration daemon running")
                 (let main-loop ()
                   (sleep 60)
                   (main-loop))))))

      (list (shepherd-service
             (provision '(opencog-orchestration))
             (documentation "OpenCog autonomous multi-agent orchestration daemon")
             (requirement '(networking))
             (modules `((opencog)
                        (opencog exec)
                        (opencog logger)
                        (opencog cogserver)
                        (opencog attention)
                        ,@%default-modules))
             (start #~(make-forkexec-constructor
                       (list #$orchestrator-script)
                       #:log-file "/var/log/opencog-orchestration.log"
                       #:environment-variables
                       (list (string-append "GUILE_LOAD_PATH="
                                          #$atomspace "/share/guile/site/2.2:"
                                          #$cogserver "/share/guile/site/2.2:"
                                          #$attention "/share/guile/site/2.2"))))
             (stop #~(make-kill-destructor)))))))

(define opencog-orchestration-service-type
  (service-type
   (name 'opencog-orchestration)
   (description
    "Run OpenCog as an autonomous multi-agent orchestration workbench,
using daemon-based architecture and pure Scheme implementation.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             opencog-orchestration-shepherd-service)))
   (default-value (opencog-orchestration-configuration))))

;;;
;;; Individual Agent Service
;;;

(define (opencog-agent-shepherd-service agent-config)
  "Return a <shepherd-service> for an individual OpenCog agent."
  (match-record agent-config <opencog-agent-configuration>
    (name module entry-point auto-start? dependencies)
    (list (shepherd-service
           (provision (list (symbol-append 'opencog-agent- name)))
           (documentation (string-append "OpenCog agent: " (symbol->string name)))
           (requirement `(opencog-orchestration ,@dependencies))
           (modules `((opencog)
                      (opencog exec)
                      ,@(if module (list module) '())
                      ,@%default-modules))
           (start #~(lambda ()
                      (format #t "Starting OpenCog agent: ~a~%" '#$name)
                      #t))
           (stop #~(lambda ()
                     (format #t "Stopping OpenCog agent: ~a~%" '#$name)
                     #t))
           (one-shot? #f)
           (respawn? #t)))))

(define opencog-agent-service-type
  (service-type
   (name 'opencog-agent)
   (description
    "Run an individual OpenCog agent as a daemon within the orchestration framework.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             opencog-agent-shepherd-service)))
   (compose concatenate)
   (extend (lambda (config agents)
             (opencog-orchestration-configuration
              (inherit config)
              (agents (append (opencog-orchestration-configuration-agents config)
                             agents)))))))
