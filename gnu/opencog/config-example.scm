;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 OpenCog Orchestration Contributors
;;;
;;; Example configuration for OpenCog Orchestration Service
;;;
;;; This file demonstrates how to configure and use the OpenCog
;;; autonomous multi-agent orchestration workbench as a daemon service.

(use-modules (gnu)
             (gnu services)
             (gnu services opencog)
             (gnu packages opencog))

;;;
;;; Example OpenCog Orchestration Configuration
;;;

(define example-opencog-orchestration-config
  (opencog-orchestration-configuration
   (port 17001)
   (log-level "INFO")
   (data-directory "/var/lib/opencog")
   (agents
    (list
     ;; Reasoning agent - performs logical inference
     (opencog-agent-configuration
      (name 'reasoning-agent)
      (module '(gnu opencog agents))
      (auto-start? #t)
      (dependencies '()))
     
     ;; Attention allocation agent - manages cognitive resources
     (opencog-agent-configuration
      (name 'attention-agent)
      (module '(gnu opencog agents))
      (auto-start? #t)
      (dependencies '()))
     
     ;; Learning agent - discovers patterns
     (opencog-agent-configuration
      (name 'learning-agent)
      (module '(gnu opencog agents))
      (auto-start? #t)
      (dependencies '(reasoning-agent)))
     
     ;; Planning agent - generates action sequences
     (opencog-agent-configuration
      (name 'planning-agent)
      (module '(gnu opencog agents))
      (auto-start? #t)
      (dependencies '(reasoning-agent attention-agent)))
     
     ;; Communication agent - coordinates between agents
     (opencog-agent-configuration
      (name 'communication-agent)
      (module '(gnu opencog agents))
      (auto-start? #t)
      (dependencies '()))))))

;;;
;;; Operating System Configuration with OpenCog
;;;

(define %opencog-services
  (list (service opencog-orchestration-service-type
                 example-opencog-orchestration-config)))

;; This can be added to your operating-system declaration:
;;
;; (operating-system
;;   ...
;;   (services (append %opencog-services
;;                     %base-services)))
