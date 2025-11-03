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
                              (ice-9 threads)
                              (ice-9 match))

                 ;; Initialize logging
                 (cog-logger-set-level! #$log-level)
                 (cog-logger-info "Starting OpenCog Orchestration Service")

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

                 ;; Initialize attention allocation
                 (cog-logger-info "Initializing attention allocation system")

                 ;; Agent registry and coordination
                 (define agent-registry (make-hash-table))
                 (define agent-states (make-hash-table))
                 (define orchestration-mutex (make-mutex))

                 ;; Agent lifecycle management
                 (define (register-agent name agent-proc dependencies)
                   (with-mutex orchestration-mutex
                     (hash-set! agent-registry name
                                (list agent-proc dependencies))
                     (hash-set! agent-states name 'registered)
                     (cog-logger-info
                      (format #f "Agent registered: ~a" name))))

                 (define (start-agent name)
                   (with-mutex orchestration-mutex
                     (let ((agent-info (hash-ref agent-registry name)))
                       (when agent-info
                         (match agent-info
                           ((agent-proc dependencies)
                            ;; Check dependencies
                            (if (every (lambda (dep)
                                        (eq? 'running
                                             (hash-ref agent-states dep)))
                                      dependencies)
                                (begin
                                  (hash-set! agent-states name 'starting)
                                  (call-with-new-thread
                                   (lambda ()
                                     (hash-set! agent-states name 'running)
                                     (cog-logger-info
                                      (format #f "Agent started: ~a" name))
                                     (agent-proc atomspace)
                                     (hash-set! agent-states name 'stopped)))
                                  #t)
                                (begin
                                  (cog-logger-warn
                                   (format #f "Dependencies not met for ~a" name))
                                  #f))))))))

                 (define (stop-agent name)
                   (with-mutex orchestration-mutex
                     (hash-set! agent-states name 'stopping)
                     (cog-logger-info
                      (format #f "Agent stopping: ~a" name))
                     ;; Agent cleanup handled by thread termination
                     (hash-set! agent-states name 'stopped)))

                 (define (get-agent-status name)
                   (hash-ref agent-states name 'unknown))

                 ;; Orchestration control loop
                 (define (orchestration-loop)
                   (let loop ()
                     ;; Monitor agent states
                     (with-mutex orchestration-mutex
                       (hash-for-each
                        (lambda (name state)
                          (when (eq? state 'error)
                            (cog-logger-error
                             (format #f "Agent in error state: ~a" name))))
                        agent-states))
                     ;; Sleep and continue
                     (sleep 5)
                     (loop)))

                 ;; Start orchestration monitoring thread
                 (call-with-new-thread orchestration-loop)

                 ;; Register configured agents
                 #$@(map (lambda (agent)
                          #~(register-agent
                             '#$(opencog-agent-configuration-name agent)
                             (lambda (as)
                               (cog-logger-info
                                (format #f "Running agent: ~a"
                                        '#$(opencog-agent-configuration-name agent)))
                               ;; Agent implementation loop
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
           (provision (list (symbol-append 'opencog-agent- (string->symbol (symbol->string name)))))
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
