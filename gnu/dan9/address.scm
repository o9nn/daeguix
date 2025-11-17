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

(define-module (gnu dan9 address)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-address
            address?
            address-uri
            address-daemon
            parse-address
            make-address-router
            address-router?
            router-register!
            router-unregister!
            router-resolve
            router-send-to-address
            address-router-daemon-loop
            
            ;; Thread pool management
            make-thread-pool
            thread-pool?
            thread-pool-size
            thread-pool-spawn-cycle!
            thread-pool-drop-cycle!
            thread-pool-sync-cycles!))

;;;
;;; D9 Address System
;;;
;;; Instead of Plan9-style namespaces, D9 uses URI-based addresses for daemon
;;; routing. This enables:
;;; - Direct daemon addressing: d9://scheduler/year-gear
;;; - Event loop subscription by address
;;; - Dynamic thread pool allocation
;;; - Instantaneous spawn/drop of cycles (1 to 10^12)
;;;

;;; Address record - represents a D9 URI address

(define-record-type <address>
  (make-address-internal uri daemon metadata)
  address?
  (uri address-uri)                     ; URI string (e.g., "d9://egregore/swarm/worker-1")
  (daemon address-daemon)               ; Daemon instance or #f
  (metadata address-metadata set-address-metadata!)) ; Additional routing info

;;; Address router record - manages address-to-daemon mappings

(define-record-type <address-router>
  (make-address-router-internal table mutex thread-pools)
  address-router?
  (table router-table)                  ; Hash table: address-uri -> address
  (mutex router-mutex)
  (thread-pools router-thread-pools set-router-thread-pools!))

;;; Thread pool record - manages execution cycles

(define-record-type <thread-pool>
  (make-thread-pool-internal name size cycles mutex)
  thread-pool?
  (name thread-pool-name)
  (size thread-pool-size set-thread-pool-size!)
  (cycles thread-pool-cycles set-thread-pool-cycles!) ; List of active cycle threads
  (mutex thread-pool-mutex))

;;;
;;; Address Creation and Parsing
;;;

(define* (make-address uri #:key (daemon #f) (metadata '()))
  "Create a D9 address with URI and optional daemon binding."
  (make-address-internal uri daemon metadata))

(define (parse-address uri-string)
  "Parse a D9 URI string and extract components.
Example: d9://egregore/swarm/worker-1
Returns: (scheme . d9) (type . egregore) (group . swarm) (name . worker-1)"
  (let ((match (string-match "^d9://([^/]+)/([^/]+)/(.+)$" uri-string)))
    (if match
        (list (cons 'scheme "d9")
              (cons 'type (match:substring match 1))
              (cons 'group (match:substring match 2))
              (cons 'name (match:substring match 3)))
        (list (cons 'scheme "d9")
              (cons 'raw uri-string)))))

;;;
;;; Address Router
;;;

(define (make-address-router)
  "Create an address router for managing D9 address mappings."
  (make-address-router-internal (make-hash-table) (make-mutex) '()))

(define (router-register! router uri daemon)
  "Register a daemon at the given URI address."
  (with-mutex (router-mutex router)
    (let ((addr (make-address uri #:daemon daemon)))
      (hash-set! (router-table router) uri addr)
      (format #t "[AddressRouter] Registered: ~a -> ~a~%"
              uri (daemon-name daemon))
      addr)))

(define (router-unregister! router uri)
  "Unregister an address from the router."
  (with-mutex (router-mutex router)
    (hash-remove! (router-table router) uri)
    (format #t "[AddressRouter] Unregistered: ~a~%" uri)))

(define (router-resolve router uri)
  "Resolve a URI to a daemon address."
  (with-mutex (router-mutex router)
    (hash-ref (router-table router) uri #f)))

(define (router-send-to-address router sender-uri target-uri msg-type payload)
  "Send a message to a daemon via URI address."
  (let ((sender-addr (router-resolve router sender-uri))
        (target-addr (router-resolve router target-uri)))
    (if (and sender-addr target-addr)
        (let ((sender-daemon (address-daemon sender-addr))
              (target-daemon (address-daemon target-addr)))
          (if (and sender-daemon target-daemon)
              (begin
                (daemon-send-message sender-daemon target-daemon msg-type payload)
                #t)
              #f))
        #f)))

;;;
;;; Address Router Daemon
;;;

(define (address-router-daemon-loop daemon)
  "Main loop for the address router daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process routing requests
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-router-message daemon message)))
      
      ;; Update metrics
      (daemon-set-metric! daemon 'last-poll (current-time time-monotonic))
      
      ;; Sleep briefly
      (usleep 10000)
      (loop))))

(define (handle-router-message daemon message)
  "Handle address router control messages."
  (match (message-type message)
    ('register (handle-router-register daemon message))
    ('unregister (handle-router-unregister daemon message))
    ('resolve (handle-router-resolve daemon message))
    ('send (handle-router-send daemon message))
    (_ (format #t "[AddressRouter] Unknown message type: ~a~%"
               (message-type message)))))

(define (handle-router-register daemon message)
  "Handle daemon registration request."
  (let* ((payload (message-payload message))
         (uri (assoc-ref payload 'uri))
         (target-daemon (assoc-ref payload 'daemon)))
    (when (and uri target-daemon)
      (format #t "[AddressRouter] Register request: ~a~%" uri))))

(define (handle-router-unregister daemon message)
  "Handle daemon unregistration request."
  (let* ((payload (message-payload message))
         (uri (assoc-ref payload 'uri)))
    (when uri
      (format #t "[AddressRouter] Unregister request: ~a~%" uri))))

(define (handle-router-resolve daemon message)
  "Handle address resolution request."
  (let* ((payload (message-payload message))
         (uri (assoc-ref payload 'uri)))
    (when uri
      (format #t "[AddressRouter] Resolve request: ~a~%" uri))))

(define (handle-router-send daemon message)
  "Handle send-to-address request."
  (let* ((payload (message-payload message))
         (target-uri (assoc-ref payload 'target-uri)))
    (when target-uri
      (format #t "[AddressRouter] Send to: ~a~%" target-uri))))

;;;
;;; Thread Pool Management
;;;

(define* (make-thread-pool name #:key (size 10))
  "Create a thread pool for managing execution cycles."
  (make-thread-pool-internal name size '() (make-mutex)))

(define (thread-pool-spawn-cycle! pool cycle-fn)
  "Spawn a new execution cycle in the thread pool.
Returns the spawned thread."
  (with-mutex (thread-pool-mutex pool)
    (let ((thread (call-with-new-thread cycle-fn)))
      (set-thread-pool-cycles! pool (cons thread (thread-pool-cycles pool)))
      (format #t "[ThreadPool:~a] Spawned cycle (total: ~a)~%"
              (thread-pool-name pool)
              (length (thread-pool-cycles pool)))
      thread)))

(define (thread-pool-drop-cycle! pool thread)
  "Drop an execution cycle from the thread pool."
  (with-mutex (thread-pool-mutex pool)
    (set-thread-pool-cycles! pool (delete thread (thread-pool-cycles pool)))
    (format #t "[ThreadPool:~a] Dropped cycle (remaining: ~a)~%"
            (thread-pool-name pool)
            (length (thread-pool-cycles pool)))))

(define (thread-pool-sync-cycles! pool target-count)
  "Synchronize thread pool to target cycle count.
Spawn or drop cycles to reach target count instantaneously."
  (with-mutex (thread-pool-mutex pool)
    (let* ((current-count (length (thread-pool-cycles pool)))
           (diff (- target-count current-count)))
      (cond
       ((> diff 0)
        ;; Spawn new cycles
        (format #t "[ThreadPool:~a] Spawning ~a cycles~%"
                (thread-pool-name pool) diff)
        (let spawn-loop ((i 0))
          (when (< i diff)
            (thread-pool-spawn-cycle! pool (lambda () (usleep 1000000)))
            (spawn-loop (+ i 1)))))
       ((< diff 0)
        ;; Drop excess cycles
        (format #t "[ThreadPool:~a] Dropping ~a cycles~%"
                (thread-pool-name pool) (abs diff))
        (let drop-loop ((i 0) (cycles (thread-pool-cycles pool)))
          (when (and (< i (abs diff)) (not (null? cycles)))
            (thread-pool-drop-cycle! pool (car cycles))
            (drop-loop (+ i 1) (cdr cycles)))))
       (else
        (format #t "[ThreadPool:~a] Already at target: ~a cycles~%"
                (thread-pool-name pool) target-count))))))
