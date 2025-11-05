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

(define-module (gnu opencog distributed)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-18)
  #:export (make-distributed-coordinator
            coordinator-register-node
            coordinator-unregister-node
            coordinator-broadcast
            coordinator-send-message
            coordinator-get-nodes
            make-styx-protocol
            styx-connect
            styx-disconnect))

;;;
;;; Inferno-Inspired Distributed Coordination
;;;
;;; Implements distributed agent coordination inspired by Inferno's
;;; Styx protocol and distributed systems architecture.
;;;

(define-record-type <distributed-node>
  (make-node id address capabilities state)
  node?
  (id node-id)
  (address node-address)
  (capabilities node-capabilities set-node-capabilities!)
  (state node-state set-node-state!))

(define-record-type <distributed-coordinator>
  (make-coordinator-internal nodes message-queue mutex condition)
  coordinator?
  (nodes coordinator-nodes set-coordinator-nodes!)
  (message-queue coordinator-queue)
  (mutex coordinator-mutex)
  (condition coordinator-condition))

(define (make-distributed-coordinator)
  "Create a distributed coordinator for multi-node agent orchestration."
  (make-coordinator-internal
   (make-hash-table)
   '()
   (make-mutex)
   (make-condition-variable)))

;;;
;;; Node Management
;;;

(define (coordinator-register-node coordinator node-id address capabilities)
  "Register a new node in the distributed system."
  (with-mutex (coordinator-mutex coordinator)
    (let ((node (make-node node-id address capabilities 'active)))
      (hash-set! (coordinator-nodes coordinator) node-id node)
      (format #t "[distributed] Node registered: ~a at ~a~%" node-id address)
      #t)))

(define (coordinator-unregister-node coordinator node-id)
  "Unregister a node from the distributed system."
  (with-mutex (coordinator-mutex coordinator)
    (hash-remove! (coordinator-nodes coordinator) node-id)
    (format #t "[distributed] Node unregistered: ~a~%" node-id)
    #t))

(define (coordinator-get-nodes coordinator)
  "Get all registered nodes."
  (with-mutex (coordinator-mutex coordinator)
    (hash-map->list
     (lambda (id node) (cons id node))
     (coordinator-nodes coordinator))))

;;;
;;; Message Passing (Inferno-style)
;;;

(define-record-type <distributed-message>
  (make-message source destination type payload timestamp)
  message?
  (source message-source)
  (destination message-destination)
  (type message-type)
  (payload message-payload)
  (timestamp message-timestamp))

(define (coordinator-send-message coordinator source dest msg-type payload)
  "Send a message to a specific node."
  (with-mutex (coordinator-mutex coordinator)
    (let ((msg (make-message source dest msg-type payload
                            (time-second (current-time time-monotonic)))))
      (format #t "[distributed] Message ~a: ~a -> ~a~%" msg-type source dest)
      ;; In a full implementation, this would handle actual network transmission
      ;; For now, we log the message
      #t)))

(define (coordinator-broadcast coordinator source msg-type payload)
  "Broadcast a message to all nodes."
  (let ((nodes (coordinator-get-nodes coordinator)))
    (for-each
     (lambda (node-pair)
       (coordinator-send-message coordinator source (car node-pair) msg-type payload))
     nodes)))

;;;
;;; Styx-Inspired Protocol for File/Resource Streaming
;;;
;;; Styx is Inferno's protocol for streaming file system operations
;;; We adapt it for streaming AtomSpace operations and agent coordination
;;;

(define-record-type <styx-connection>
  (make-styx-connection-internal id local-addr remote-addr stream-state mutex)
  styx-connection?
  (id styx-id)
  (local-addr styx-local)
  (remote-addr styx-remote)
  (stream-state styx-state set-styx-state!)
  (mutex styx-mutex))

(define-record-type <styx-protocol>
  (make-styx-protocol-internal connections handlers mutex)
  styx-protocol?
  (connections styx-connections set-styx-connections!)
  (handlers styx-handlers)
  (mutex styx-mutex))

(define (make-styx-protocol)
  "Create a Styx-style protocol handler for streaming operations."
  (make-styx-protocol-internal
   (make-hash-table)
   (make-hash-table)
   (make-mutex)))

(define (styx-connect protocol local-addr remote-addr)
  "Establish a Styx connection to a remote node."
  (with-mutex (styx-mutex protocol)
    (let* ((conn-id (string-append local-addr ":" remote-addr))
           (conn (make-styx-connection-internal
                  conn-id
                  local-addr
                  remote-addr
                  'connected
                  (make-mutex))))
      (hash-set! (styx-connections protocol) conn-id conn)
      (format #t "[styx] Connection established: ~a~%" conn-id)
      conn)))

(define (styx-disconnect protocol connection)
  "Close a Styx connection."
  (with-mutex (styx-mutex protocol)
    (hash-remove! (styx-connections protocol) (styx-id connection))
    (set-styx-state! connection 'disconnected)
    (format #t "[styx] Connection closed: ~a~%" (styx-id connection))
    #t))

;;;
;;; Load Balancing and Task Distribution
;;;

(define (distribute-task coordinator task-type payload)
  "Distribute a task across available nodes using Inferno-style load balancing."
  (let* ((nodes (coordinator-get-nodes coordinator))
         (active-nodes (filter (lambda (n) (eq? 'active (node-state (cdr n)))) nodes)))
    (if (null? active-nodes)
        (begin
          (format #t "[distributed] No active nodes for task distribution~%")
          #f)
        (let ((target-node (car active-nodes))) ;; Simple round-robin
          (coordinator-send-message coordinator 'orchestrator (car target-node)
                                   task-type payload)
          (format #t "[distributed] Task ~a distributed to ~a~%"
                  task-type (car target-node))
          #t))))

;;;
;;; Distributed AtomSpace Synchronization
;;;

(define (sync-atomspace-delta coordinator source-node atoms)
  "Synchronize AtomSpace changes across distributed nodes."
  (coordinator-broadcast coordinator source-node 'atomspace-sync
                        (list 'delta atoms (time-second (current-time time-monotonic)))))

(define (replicate-atomspace coordinator source target)
  "Replicate entire AtomSpace from source to target node."
  (coordinator-send-message coordinator source target 'atomspace-replicate
                          (list 'full-sync (time-second (current-time time-monotonic)))))
