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

(define-module (gnu opencog orchestration)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-18)
  #:use-module (gnu opencog namespaces)
  #:use-module (gnu opencog distributed)
  #:use-module (gnu opencog daemons)
  #:export (make-orchestrator
            orchestrator-register-agent
            orchestrator-start-agent
            orchestrator-stop-agent
            orchestrator-get-status
            orchestrator-run
            orchestrator-add-daemon
            orchestrator-get-namespace))

;;;
;;; Orchestrator - Pure Scheme Multi-Agent Coordination
;;;

(define-record-type <orchestrator>
  (make-orchestrator-internal atomspace agents mutex scheduler namespace
                             coordinator daemons 9p-service)
  orchestrator?
  (atomspace orchestrator-atomspace)
  (agents orchestrator-agents set-orchestrator-agents!)
  (mutex orchestrator-mutex)
  (scheduler orchestrator-scheduler set-orchestrator-scheduler!)
  (namespace orchestrator-namespace)
  (coordinator orchestrator-coordinator)
  (daemons orchestrator-daemons set-orchestrator-daemons!)
  (9p-service orchestrator-9p-service))

(define* (make-orchestrator atomspace)
  "Create a new orchestrator for managing multiple agents with Plan9/Inferno features."
  (let* ((namespace (make-namespace-manager #:root "/opencog"))
         (coordinator (make-distributed-coordinator))
         (9p-svc (make-9p-service namespace)))
    
    ;; Initialize standard namespace paths (Plan9 style)
    (namespace-bind namespace "/agents" 'directory #f '((type . agents)))
    (namespace-bind namespace "/atomspace" 'directory #f '((type . atomspace)))
    (namespace-bind namespace "/services" 'directory #f '((type . services)))
    (namespace-bind namespace "/proc" 'directory #f '((type . proc)))
    (namespace-bind namespace "/dev" 'directory #f '((type . devices)))
    (namespace-bind namespace "/net" 'directory #f '((type . network)))
    
    (make-orchestrator-internal
     atomspace
     (make-hash-table)
     (make-mutex)
     #f
     namespace
     coordinator
     '()
     9p-svc)))

;;;
;;; Agent Registration and Management
;;;

(define (orchestrator-register-agent orchestrator name agent-proc dependencies)
  "Register an agent with the orchestrator and bind it to namespace."
  (with-mutex (orchestrator-mutex orchestrator)
    (hash-set! (orchestrator-agents orchestrator)
               name
               (list 'registered agent-proc dependencies '()))
    
    ;; Bind agent to namespace (Plan9-style)
    (let ((agent-path (string-append "/agents/" (symbol->string name))))
      (namespace-bind (orchestrator-namespace orchestrator)
                     agent-path
                     'agent
                     agent-proc
                     (list (cons 'name name)
                           (cons 'dependencies dependencies)
                           (cons 'state 'registered))))))

(define (orchestrator-start-agent orchestrator name)
  "Start a registered agent."
  (with-mutex (orchestrator-mutex orchestrator)
    (let ((agent-info (hash-ref (orchestrator-agents orchestrator) name)))
      (when agent-info
        (match agent-info
          ((state agent-proc dependencies thread)
           ;; Check if dependencies are running
           (let ((deps-ready?
                  (every (lambda (dep)
                          (let ((dep-info (hash-ref (orchestrator-agents orchestrator) dep)))
                            (and dep-info
                                 (eq? 'running (car dep-info)))))
                        dependencies)))
             (if deps-ready?
                 (let ((new-thread
                        (call-with-new-thread
                         (lambda ()
                           (agent-proc (orchestrator-atomspace orchestrator))))))
                   (hash-set! (orchestrator-agents orchestrator)
                             name
                             (list 'running agent-proc dependencies new-thread))
                   (format #t "Agent ~a started~%" name)
                   #t)
                 (begin
                   (format #t "Dependencies not ready for agent ~a~%" name)
                   #f)))))))))

(define (orchestrator-stop-agent orchestrator name)
  "Stop a running agent."
  (with-mutex (orchestrator-mutex orchestrator)
    (let ((agent-info (hash-ref (orchestrator-agents orchestrator) name)))
      (when agent-info
        (match agent-info
          ((state agent-proc dependencies thread)
           (when (and (eq? state 'running) thread)
             ;; Note: Guile doesn't provide safe thread cancellation.
             ;; Agents should implement cooperative termination by
             ;; checking a termination flag. Thread cleanup is handled
             ;; by Guile's garbage collector when references are dropped.
             (hash-set! (orchestrator-agents orchestrator)
                       name
                       (list 'stopped agent-proc dependencies #f))
             (format #t "Agent ~a stopped~%" name)
             #t)))))))

(define (orchestrator-get-status orchestrator)
  "Get the status of all agents in the orchestrator."
  (with-mutex (orchestrator-mutex orchestrator)
    (hash-map->list
     (lambda (name info)
       (cons name (car info)))
     (orchestrator-agents orchestrator))))

;;;
;;; Orchestrator Main Loop
;;;

(define (orchestrator-run orchestrator)
  "Run the orchestrator's main monitoring and coordination loop with daemon support."
  (let loop ()
    ;; Monitor agent health
    (let ((statuses (orchestrator-get-status orchestrator)))
      (for-each
       (lambda (status)
         (when (eq? (cdr status) 'error)
           (format #t "WARNING: Agent ~a in error state~%" (car status))))
       statuses))
    
    ;; Check daemon status
    (for-each
     (lambda (daemon)
       (let ((status (daemon-status daemon)))
         (unless (eq? 'running (assoc-ref status 'state))
           (format #t "WARNING: Daemon ~a not running~%"
                   (assoc-ref status 'name)))))
     (orchestrator-daemons orchestrator))
    
    ;; Perform coordination tasks
    (sleep 5)
    (loop)))

;;;
;;; Daemon Management
;;;

(define (orchestrator-add-daemon orchestrator daemon)
  "Add and start a daemon in the orchestrator."
  (with-mutex (orchestrator-mutex orchestrator)
    (set-orchestrator-daemons! orchestrator
                              (cons daemon (orchestrator-daemons orchestrator)))
    (daemon-start daemon)
    (format #t "[orchestrator] Added daemon: ~a~%" (daemon-name daemon))
    #t))

(define (orchestrator-get-namespace orchestrator)
  "Get the orchestrator's namespace manager."
  (orchestrator-namespace orchestrator))
