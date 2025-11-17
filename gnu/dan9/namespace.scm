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

(define-module (gnu dan9 namespace)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (make-namespace-daemon
            namespace-daemon-loop))

;;;
;;; Namespace Daemon
;;;
;;; In dan9, namespaces are managed by a daemon. Like plan9's namespace
;;; management, but instead of mounting files, we attach daemons to names.
;;; This daemon maintains the mapping of names to daemon services.
;;;

(define* (make-namespace-daemon #:key
                               (name "namespace"))
  "Create a namespace management daemon."
  (make-daemon name 'namespace
               #:config (list (cons 'namespace-table (make-hash-table)))
               #:loop-fn namespace-daemon-loop))

(define (namespace-daemon-loop daemon)
  "Main loop for the namespace daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Check for incoming messages
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-namespace-message daemon message)))
      
      ;; Update metrics
      (daemon-set-metric! daemon 'last-poll (current-time time-monotonic))
      
      ;; Sleep briefly
      (usleep 10000) ; 10ms
      (loop))))

(define (handle-namespace-message daemon message)
  "Handle a namespace operation message."
  (match (message-type message)
    ('bind (handle-bind daemon message))
    ('unbind (handle-unbind daemon message))
    ('lookup (handle-lookup daemon message))
    ('list (handle-list-namespace daemon message))
    ('mount (handle-mount daemon message))
    ('unmount (handle-unmount daemon message))
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type message)))))

(define (handle-bind daemon message)
  "Handle a daemon binding request."
  (let* ((payload (message-payload message))
         (path (assoc-ref payload 'path))
         (daemon-name (assoc-ref payload 'daemon-name))
         (namespace-table (assoc-ref (daemon-config daemon) 'namespace-table)))
    (format #t "[~a] Bind ~a to ~a~%"
            (daemon-name daemon) path daemon-name)
    (hash-set! namespace-table path daemon-name)
    (daemon-set-metric! daemon 'bind-count
                       (+ 1 (or (daemon-get-metric daemon 'bind-count) 0)))))

(define (handle-unbind daemon message)
  "Handle a daemon unbinding request."
  (let* ((payload (message-payload message))
         (path (assoc-ref payload 'path))
         (namespace-table (assoc-ref (daemon-config daemon) 'namespace-table)))
    (format #t "[~a] Unbind ~a~%"
            (daemon-name daemon) path)
    (hash-remove! namespace-table path)
    (daemon-set-metric! daemon 'unbind-count
                       (+ 1 (or (daemon-get-metric daemon 'unbind-count) 0)))))

(define (handle-lookup daemon message)
  "Handle a namespace lookup request."
  (let* ((payload (message-payload message))
         (path (assoc-ref payload 'path))
         (namespace-table (assoc-ref (daemon-config daemon) 'namespace-table))
         (result (hash-ref namespace-table path #f)))
    (format #t "[~a] Lookup ~a -> ~a~%"
            (daemon-name daemon) path (or result "not found"))
    (daemon-set-metric! daemon 'lookup-count
                       (+ 1 (or (daemon-get-metric daemon 'lookup-count) 0)))))

(define (handle-list-namespace daemon message)
  "Handle a namespace list request."
  (let* ((namespace-table (assoc-ref (daemon-config daemon) 'namespace-table))
         (entries (hash-count (const #t) namespace-table)))
    (format #t "[~a] List namespace (~a entries)~%"
            (daemon-name daemon) entries)
    (daemon-set-metric! daemon 'list-count
                       (+ 1 (or (daemon-get-metric daemon 'list-count) 0)))))

(define (handle-mount daemon message)
  "Handle a daemon mount request."
  (let* ((payload (message-payload message))
         (path (assoc-ref payload 'path))
         (daemon-name (assoc-ref payload 'daemon-name))
         (options (assoc-ref payload 'options)))
    (format #t "[~a] Mount ~a at ~a with options ~a~%"
            (daemon-name daemon) daemon-name path options)
    (daemon-set-metric! daemon 'mount-count
                       (+ 1 (or (daemon-get-metric daemon 'mount-count) 0)))))

(define (handle-unmount daemon message)
  "Handle a daemon unmount request."
  (let* ((payload (message-payload message))
         (path (assoc-ref payload 'path)))
    (format #t "[~a] Unmount ~a~%"
            (daemon-name daemon) path)
    (daemon-set-metric! daemon 'unmount-count
                       (+ 1 (or (daemon-get-metric daemon 'unmount-count) 0)))))
