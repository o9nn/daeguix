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

(define-module (test-services-opencog)
  #:use-module (gnu services)
  #:use-module (gnu services opencog)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages opencog)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "services-opencog")

(test-assert "opencog-orchestration-configuration, default values"
  (let ((config (opencog-orchestration-configuration)))
    (and (opencog-orchestration-configuration? config)
         (= (opencog-orchestration-configuration-port config) 17001)
         (string=? (opencog-orchestration-configuration-log-level config) "INFO")
         (string=? (opencog-orchestration-configuration-data-directory config)
                   "/var/lib/opencog")
         (null? (opencog-orchestration-configuration-agents config)))))

(test-assert "opencog-orchestration-configuration, custom values"
  (let ((config (opencog-orchestration-configuration
                 (port 18001)
                 (log-level "DEBUG")
                 (data-directory "/tmp/opencog-test"))))
    (and (= (opencog-orchestration-configuration-port config) 18001)
         (string=? (opencog-orchestration-configuration-log-level config) "DEBUG")
         (string=? (opencog-orchestration-configuration-data-directory config)
                   "/tmp/opencog-test"))))

(test-assert "opencog-agent-configuration, default values"
  (let ((agent (opencog-agent-configuration
                (name 'test-agent)
                (module '(test module)))))
    (and (opencog-agent-configuration? agent)
         (eq? (opencog-agent-configuration-name agent) 'test-agent)
         (equal? (opencog-agent-configuration-module agent) '(test module))
         (opencog-agent-configuration-auto-start? agent)
         (null? (opencog-agent-configuration-dependencies agent)))))

(test-assert "opencog-agent-configuration, with dependencies"
  (let ((agent (opencog-agent-configuration
                (name 'dependent-agent)
                (module '(test module))
                (dependencies '(agent-1 agent-2))
                (auto-start? #f))))
    (and (eq? (opencog-agent-configuration-name agent) 'dependent-agent)
         (equal? (opencog-agent-configuration-dependencies agent)
                 '(agent-1 agent-2))
         (not (opencog-agent-configuration-auto-start? agent)))))

(test-assert "opencog-orchestration-configuration with agents"
  (let* ((agent1 (opencog-agent-configuration
                  (name 'reasoning-agent)
                  (module '(gnu opencog agents))))
         (agent2 (opencog-agent-configuration
                  (name 'learning-agent)
                  (module '(gnu opencog agents))
                  (dependencies '(reasoning-agent))))
         (config (opencog-orchestration-configuration
                  (agents (list agent1 agent2)))))
    (and (= (length (opencog-orchestration-configuration-agents config)) 2)
         (eq? (opencog-agent-configuration-name
               (first (opencog-orchestration-configuration-agents config)))
              'reasoning-agent)
         (eq? (opencog-agent-configuration-name
               (second (opencog-orchestration-configuration-agents config)))
              'learning-agent))))

(test-assert "opencog-orchestration-service-type exists"
  (service-type? opencog-orchestration-service-type))

(test-assert "opencog-agent-service-type exists"
  (service-type? opencog-agent-service-type))

(test-equal "opencog-orchestration-service-type, default service"
  '(opencog-orchestration)
  (let* ((service (service opencog-orchestration-service-type))
         (shepherd-services
          (lookup-service-value service shepherd-root-service-type)))
    (map shepherd-service-provision
         (filter (lambda (s)
                  (memq 'opencog-orchestration
                        (shepherd-service-provision s)))
                shepherd-services))))

(test-assert "opencog-orchestration-service generates shepherd service"
  (let* ((config (opencog-orchestration-configuration
                  (port 17002)))
         (service (service opencog-orchestration-service-type config))
         (shepherd-services
          (lookup-service-value service shepherd-root-service-type)))
    (and (not (null? shepherd-services))
         (any (lambda (s)
               (memq 'opencog-orchestration (shepherd-service-provision s)))
             shepherd-services))))

(test-assert "opencog-orchestration requires networking"
  (let* ((service (service opencog-orchestration-service-type))
         (shepherd-services
          (lookup-service-value service shepherd-root-service-type))
         (orchestration-service
          (find (lambda (s)
                 (memq 'opencog-orchestration (shepherd-service-provision s)))
               shepherd-services)))
    (and orchestration-service
         (memq 'networking (shepherd-service-requirement orchestration-service)))))

(test-end "services-opencog")

;; Helper to look up service values across extensions
(define (lookup-service-value service target-type)
  "Look up the value provided by SERVICE for TARGET-TYPE."
  (let ((services (list service)))
    (fold-services services #:target-type target-type)))
