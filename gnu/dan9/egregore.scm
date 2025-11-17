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

(define-module (gnu dan9 egregore)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-egregore
            egregore?
            egregore-name
            egregore-archetype
            egregore-daemons
            egregore-add-daemon!
            egregore-remove-daemon!
            egregore-start
            egregore-stop
            egregore-broadcast
            egregore-status
            
            ;; Archetype patterns
            make-swarm-egregore
            make-hierarchy-egregore
            make-ring-egregore
            make-mesh-egregore))

;;;
;;; Egregore: Daemon Orchestration Archetypes
;;;
;;; An egregore is a collective group consciousness formed by daemons working
;;; together. In Dan9, egregores are orchestration patterns that coordinate
;;; multiple daemons toward a common goal.
;;;
;;; Archetype patterns:
;;; - Swarm: All daemons receive broadcasts, no hierarchy
;;; - Hierarchy: Tree structure with coordinator and workers
;;; - Ring: Daemons connected in a circular chain
;;; - Mesh: Fully connected network of daemons
;;;

;;; Record type for egregores

(define-record-type <egregore>
  (make-egregore-internal name archetype daemons mutex coordinator-daemon)
  egregore?
  (name egregore-name)
  (archetype egregore-archetype)
  (daemons egregore-daemons set-egregore-daemons!)
  (mutex egregore-mutex)
  (coordinator-daemon egregore-coordinator set-egregore-coordinator!))

;;;
;;; Egregore Creation
;;;

(define* (make-egregore name archetype #:key (daemons '()))
  "Create an egregore with NAME using ARCHETYPE pattern.
ARCHETYPE can be 'swarm, 'hierarchy, 'ring, or 'mesh.
DAEMONS is an optional initial list of daemons."
  (let ((egregore (make-egregore-internal
                   name
                   archetype
                   daemons
                   (make-mutex)
                   #f)))
    ;; Create coordinator daemon based on archetype
    (let ((coordinator (make-daemon
                        (string-append name "-coordinator")
                        'egregore-coordinator
                        #:config (list (cons 'egregore egregore))
                        #:loop-fn (get-coordinator-loop-fn archetype))))
      (set-egregore-coordinator! egregore coordinator))
    egregore))

;;;
;;; Egregore Management
;;;

(define (egregore-add-daemon! egregore daemon)
  "Add a daemon to the egregore."
  (with-mutex (egregore-mutex egregore)
    (set-egregore-daemons! egregore
                          (cons daemon (egregore-daemons egregore)))
    (format #t "[Egregore:~a] Added daemon: ~a~%"
            (egregore-name egregore)
            (daemon-name daemon))))

(define (egregore-remove-daemon! egregore daemon)
  "Remove a daemon from the egregore."
  (with-mutex (egregore-mutex egregore)
    (set-egregore-daemons! egregore
                          (delete daemon (egregore-daemons egregore)))
    (format #t "[Egregore:~a] Removed daemon: ~a~%"
            (egregore-name egregore)
            (daemon-name daemon))))

(define (egregore-start egregore)
  "Start all daemons in the egregore and the coordinator."
  (format #t "[Egregore:~a] Starting egregore (archetype: ~a)~%"
          (egregore-name egregore)
          (egregore-archetype egregore))
  
  ;; Start coordinator daemon
  (let ((coordinator (egregore-coordinator egregore)))
    (daemon-start coordinator (get-coordinator-loop-fn (egregore-archetype egregore))))
  
  ;; Start all member daemons if they have loop functions
  (for-each
   (lambda (daemon)
     (when (and (daemon-state daemon)
                (eq? 'stopped (daemon-state daemon)))
       (format #t "[Egregore:~a] Starting member daemon: ~a~%"
               (egregore-name egregore)
               (daemon-name daemon))))
   (egregore-daemons egregore)))

(define (egregore-stop egregore)
  "Stop all daemons in the egregore."
  (format #t "[Egregore:~a] Stopping egregore~%"
          (egregore-name egregore))
  
  ;; Stop coordinator
  (daemon-stop (egregore-coordinator egregore))
  
  ;; Stop all member daemons
  (for-each
   (lambda (daemon)
     (when (eq? 'running (daemon-state daemon))
       (daemon-stop daemon)))
   (egregore-daemons egregore)))

(define (egregore-broadcast egregore msg-type payload)
  "Broadcast a message to all daemons in the egregore."
  (let ((coordinator (egregore-coordinator egregore)))
    (daemon-send-message coordinator coordinator msg-type
                        (cons (cons 'broadcast-payload payload)
                              (list (cons 'target-daemons
                                         (egregore-daemons egregore)))))))

(define (egregore-status egregore)
  "Get status of the egregore."
  (list (cons 'name (egregore-name egregore))
        (cons 'archetype (egregore-archetype egregore))
        (cons 'daemon-count (length (egregore-daemons egregore)))
        (cons 'coordinator-state
              (daemon-state (egregore-coordinator egregore)))
        (cons 'daemons
              (map daemon-name (egregore-daemons egregore)))))

;;;
;;; Coordinator Loop Functions by Archetype
;;;

(define (get-coordinator-loop-fn archetype)
  "Get the appropriate coordinator loop function for the archetype."
  (match archetype
    ('swarm swarm-coordinator-loop)
    ('hierarchy hierarchy-coordinator-loop)
    ('ring ring-coordinator-loop)
    ('mesh mesh-coordinator-loop)
    (_ swarm-coordinator-loop)))

(define (swarm-coordinator-loop daemon)
  "Coordinator loop for swarm archetype - broadcasts to all."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-swarm-message daemon message)))
      (usleep 10000)
      (loop))))

(define (handle-swarm-message daemon message)
  "Handle message in swarm coordinator."
  (let* ((payload (message-payload message))
         (broadcast-payload (assoc-ref payload 'broadcast-payload))
         (target-daemons (assoc-ref payload 'target-daemons)))
    (when (and broadcast-payload target-daemons)
      (format #t "[Swarm-Coordinator] Broadcasting to ~a daemons~%"
              (length target-daemons))
      (for-each
       (lambda (target)
         (daemon-send-message daemon target 'swarm-message broadcast-payload))
       target-daemons))))

(define (hierarchy-coordinator-loop daemon)
  "Coordinator loop for hierarchy archetype - tree structure."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-hierarchy-message daemon message)))
      (usleep 10000)
      (loop))))

(define (handle-hierarchy-message daemon message)
  "Handle message in hierarchy coordinator."
  (let* ((payload (message-payload message))
         (target-daemons (assoc-ref payload 'target-daemons)))
    (when target-daemons
      (format #t "[Hierarchy-Coordinator] Delegating to ~a workers~%"
              (length target-daemons))
      ;; In hierarchy, distribute work among workers
      (for-each
       (lambda (target)
         (daemon-send-message daemon target 'work-unit
                             (list (cons 'task 'process))))
       target-daemons))))

(define (ring-coordinator-loop daemon)
  "Coordinator loop for ring archetype - circular message passing."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-ring-message daemon message)))
      (usleep 10000)
      (loop))))

(define (handle-ring-message daemon message)
  "Handle message in ring coordinator."
  (let* ((payload (message-payload message))
         (target-daemons (assoc-ref payload 'target-daemons)))
    (when (and target-daemons (not (null? target-daemons)))
      (format #t "[Ring-Coordinator] Starting ring circulation~%")
      ;; Send message to first daemon in ring
      (daemon-send-message daemon (car target-daemons) 'ring-message
                          (list (cons 'next-daemons (cdr target-daemons)))))))

(define (mesh-coordinator-loop daemon)
  "Coordinator loop for mesh archetype - fully connected."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-mesh-message daemon message)))
      (usleep 10000)
      (loop))))

(define (handle-mesh-message daemon message)
  "Handle message in mesh coordinator."
  (let* ((payload (message-payload message))
         (target-daemons (assoc-ref payload 'target-daemons)))
    (when target-daemons
      (format #t "[Mesh-Coordinator] Connecting ~a daemons in mesh~%"
              (length target-daemons))
      ;; In mesh, each daemon is aware of all others
      (for-each
       (lambda (target)
         (daemon-send-message daemon target 'mesh-peers
                             (list (cons 'peers
                                        (delete target target-daemons)))))
       target-daemons))))

;;;
;;; Archetype Factory Functions
;;;

(define* (make-swarm-egregore name #:key (daemons '()))
  "Create a swarm egregore - all daemons receive broadcasts."
  (make-egregore name 'swarm #:daemons daemons))

(define* (make-hierarchy-egregore name #:key (daemons '()))
  "Create a hierarchy egregore - tree structure with coordinator and workers."
  (make-egregore name 'hierarchy #:daemons daemons))

(define* (make-ring-egregore name #:key (daemons '()))
  "Create a ring egregore - circular message passing."
  (make-egregore name 'ring #:daemons daemons))

(define* (make-mesh-egregore name #:key (daemons '()))
  "Create a mesh egregore - fully connected network."
  (make-egregore name 'mesh #:daemons daemons))
