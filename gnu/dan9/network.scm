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

(define-module (gnu dan9 network)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (make-network-daemon
            network-daemon-loop))

;;;
;;; Network Daemon
;;;
;;; In dan9, network operations are handled by a daemon. Instead of direct
;;; socket calls, clients send messages to the network daemon requesting
;;; connections, data transfers, etc.
;;;

(define* (make-network-daemon #:key
                             (name "network")
                             (max-connections 100))
  "Create a network daemon that handles network operations."
  (make-daemon name 'network
               #:config (list (cons 'max-connections max-connections))
               #:loop-fn network-daemon-loop))

(define (network-daemon-loop daemon)
  "Main loop for the network daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Check for incoming messages
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-network-message daemon message)))
      
      ;; Update metrics
      (daemon-set-metric! daemon 'last-poll (current-time time-monotonic))
      
      ;; Sleep briefly
      (usleep 10000) ; 10ms
      (loop))))

(define (handle-network-message daemon message)
  "Handle a network operation message."
  (match (message-type message)
    ('connect (handle-connect daemon message))
    ('disconnect (handle-disconnect daemon message))
    ('send (handle-send daemon message))
    ('receive (handle-receive daemon message))
    ('listen (handle-listen daemon message))
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type message)))))

(define (handle-connect daemon message)
  "Handle a connection request."
  (let* ((payload (message-payload message))
         (host (assoc-ref payload 'host))
         (port (assoc-ref payload 'port)))
    (format #t "[~a] Connect request to ~a:~a~%"
            (daemon-name daemon) host port)
    (daemon-set-metric! daemon 'connect-count
                       (+ 1 (or (daemon-get-metric daemon 'connect-count) 0)))))

(define (handle-disconnect daemon message)
  "Handle a disconnection request."
  (let* ((payload (message-payload message))
         (connection-id (assoc-ref payload 'connection-id)))
    (format #t "[~a] Disconnect request for connection ~a~%"
            (daemon-name daemon) connection-id)
    (daemon-set-metric! daemon 'disconnect-count
                       (+ 1 (or (daemon-get-metric daemon 'disconnect-count) 0)))))

(define (handle-send daemon message)
  "Handle a data send request."
  (let* ((payload (message-payload message))
         (connection-id (assoc-ref payload 'connection-id))
         (data (assoc-ref payload 'data)))
    (format #t "[~a] Send request on connection ~a (~a bytes)~%"
            (daemon-name daemon) connection-id (string-length (or data "")))
    (daemon-set-metric! daemon 'send-count
                       (+ 1 (or (daemon-get-metric daemon 'send-count) 0)))))

(define (handle-receive daemon message)
  "Handle a data receive request."
  (let* ((payload (message-payload message))
         (connection-id (assoc-ref payload 'connection-id)))
    (format #t "[~a] Receive request on connection ~a~%"
            (daemon-name daemon) connection-id)
    (daemon-set-metric! daemon 'receive-count
                       (+ 1 (or (daemon-get-metric daemon 'receive-count) 0)))))

(define (handle-listen daemon message)
  "Handle a listen request."
  (let* ((payload (message-payload message))
         (port (assoc-ref payload 'port)))
    (format #t "[~a] Listen request on port ~a~%"
            (daemon-name daemon) port)
    (daemon-set-metric! daemon 'listen-count
                       (+ 1 (or (daemon-get-metric daemon 'listen-count) 0)))))
