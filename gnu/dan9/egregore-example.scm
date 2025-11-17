#!/usr/bin/env -S guile --no-auto-compile
!#
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Dan9 Contributors
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

;;;
;;; Egregore Example: Daemon Orchestration Archetypes
;;;
;;; This example demonstrates the four egregore archetypes for orchestrating
;;; groups of daemons: swarm, hierarchy, ring, and mesh.
;;;

(add-to-load-path (dirname (current-filename)))

(use-modules (gnu dan9 daemons)
             (gnu dan9 egregore)
             (gnu dan9 filesystem)
             (gnu dan9 network)
             (gnu dan9 process))

(define (main)
  (display "\n=== Egregore: Daemon Orchestration Archetypes ===\n\n")
  
  (display "An egregore is a collective group consciousness formed by\n")
  (display "daemons working together toward a common goal.\n\n")
  
  (display "Four orchestration archetypes:\n")
  (display "  1. Swarm: All daemons receive broadcasts, no hierarchy\n")
  (display "  2. Hierarchy: Tree structure with coordinator and workers\n")
  (display "  3. Ring: Daemons connected in a circular chain\n")
  (display "  4. Mesh: Fully connected network of daemons\n\n")
  
  ;; Create worker daemons
  (display "Creating worker daemons...\n")
  (define worker1 (make-daemon "worker-1" 'worker #:auto-register #f))
  (define worker2 (make-daemon "worker-2" 'worker #:auto-register #f))
  (define worker3 (make-daemon "worker-3" 'worker #:auto-register #f))
  (define worker4 (make-daemon "worker-4" 'worker #:auto-register #f))
  (display "  ✓ Created 4 worker daemons\n\n")
  
  ;; Demonstrate Swarm Egregore
  (display "=== 1. Swarm Egregore ===\n")
  (display "Creating swarm egregore...\n")
  (define swarm (make-swarm-egregore "data-processors"
                                    #:daemons (list worker1 worker2)))
  (egregore-start swarm)
  (sleep 0.2)
  
  (display "Broadcasting message to swarm...\n")
  (egregore-broadcast swarm 'process-data '((dataset . "users.csv")))
  (sleep 0.3)
  
  (let ((status (egregore-status swarm)))
    (format #t "Swarm status: ~a daemons, archetype: ~a~%"
            (assoc-ref status 'daemon-count)
            (assoc-ref status 'archetype)))
  
  (egregore-stop swarm)
  (display "  ✓ Swarm demonstration complete\n\n")
  
  ;; Demonstrate Hierarchy Egregore
  (display "=== 2. Hierarchy Egregore ===\n")
  (display "Creating hierarchy egregore...\n")
  (define hierarchy (make-hierarchy-egregore "task-delegator"
                                            #:daemons (list worker1 worker2 worker3)))
  (egregore-start hierarchy)
  (sleep 0.2)
  
  (display "Delegating tasks through hierarchy...\n")
  (egregore-broadcast hierarchy 'delegate-work '((job . "analyze-logs")))
  (sleep 0.3)
  
  (let ((status (egregore-status hierarchy)))
    (format #t "Hierarchy status: ~a workers under coordinator~%"
            (assoc-ref status 'daemon-count)))
  
  (egregore-stop hierarchy)
  (display "  ✓ Hierarchy demonstration complete\n\n")
  
  ;; Demonstrate Ring Egregore
  (display "=== 3. Ring Egregore ===\n")
  (display "Creating ring egregore...\n")
  (define ring (make-ring-egregore "pipeline"
                                  #:daemons (list worker1 worker2 worker3 worker4)))
  (egregore-start ring)
  (sleep 0.2)
  
  (display "Starting ring circulation...\n")
  (egregore-broadcast ring 'circulate '((token . "process-token")))
  (sleep 0.3)
  
  (let ((status (egregore-status ring)))
    (format #t "Ring status: ~a daemons in circular chain~%"
            (assoc-ref status 'daemon-count)))
  
  (egregore-stop ring)
  (display "  ✓ Ring demonstration complete\n\n")
  
  ;; Demonstrate Mesh Egregore
  (display "=== 4. Mesh Egregore ===\n")
  (display "Creating mesh egregore...\n")
  (define mesh (make-mesh-egregore "consensus-network"
                                  #:daemons (list worker1 worker2 worker3)))
  (egregore-start mesh)
  (sleep 0.2)
  
  (display "Connecting daemons in mesh...\n")
  (egregore-broadcast mesh 'sync-state '((state . "initial")))
  (sleep 0.3)
  
  (let ((status (egregore-status mesh)))
    (format #t "Mesh status: ~a fully-connected daemons~%"
            (assoc-ref status 'daemon-count)))
  
  (egregore-stop mesh)
  (display "  ✓ Mesh demonstration complete\n\n")
  
  ;; Complex example: Combining egregores with service daemons
  (display "=== 5. Complex Example: Service Egregore ===\n")
  (display "Creating service daemons...\n")
  (define fs-daemon (make-filesystem-daemon #:name "fs-service"))
  (define net-daemon (make-network-daemon #:name "net-service"))
  (define proc-daemon (make-process-daemon #:name "proc-service"))
  (display "  ✓ Created filesystem, network, and process daemons\n")
  
  (display "Creating service mesh egregore...\n")
  (define services (make-mesh-egregore "core-services"
                                      #:daemons (list fs-daemon net-daemon proc-daemon)))
  (egregore-start services)
  (sleep 0.2)
  
  (display "Broadcasting health check to all services...\n")
  (egregore-broadcast services 'health-check '((requester . "monitor")))
  (sleep 0.3)
  
  (let ((status (egregore-status services)))
    (format #t "Service egregore: ~a services interconnected~%"
            (assoc-ref status 'daemon-count))
    (format #t "Services: ~a~%" (assoc-ref status 'daemons)))
  
  (egregore-stop services)
  (display "  ✓ Service egregore demonstration complete\n\n")
  
  ;; Summary
  (display "=== Egregore Archetypes Summary ===\n\n")
  
  (display "Swarm Archetype:\n")
  (display "  - Best for: Broadcasting updates, parallel processing\n")
  (display "  - Pattern: All daemons equal, all receive messages\n")
  (display "  - Example: Data processors, worker pools\n\n")
  
  (display "Hierarchy Archetype:\n")
  (display "  - Best for: Task delegation, tree-structured workflows\n")
  (display "  - Pattern: Coordinator distributes work to workers\n")
  (display "  - Example: Job scheduling, divide-and-conquer tasks\n\n")
  
  (display "Ring Archetype:\n")
  (display "  - Best for: Pipeline processing, token passing\n")
  (display "  - Pattern: Message flows in circular chain\n")
  (display "  - Example: Processing pipelines, leader election\n\n")
  
  (display "Mesh Archetype:\n")
  (display "  - Best for: Consensus, peer-to-peer coordination\n")
  (display "  - Pattern: All daemons aware of all others\n")
  (display "  - Example: Distributed consensus, service discovery\n\n")
  
  (display "=== Egregore Example Complete ===\n\n"))

;; Run the example
(main)
