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

(define-module (gnu opencog namespaces)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (make-namespace-manager
            namespace-bind
            namespace-unbind
            namespace-lookup
            namespace-mount
            namespace-unmount
            namespace-walk
            make-9p-service
            9p-service-start
            9p-service-stop))

;;;
;;; Plan9-Inspired Namespace Management
;;; 
;;; Implements a hierarchical namespace system inspired by Plan9
;;; where resources (agents, atomspaces, services) are accessible
;;; through a unified filesystem-like interface.
;;;

;;; Constants

(define DEFAULT-9P-PORT 9564)
(define DEFAULT-NAMESPACE-ROOT "/")

(define-record-type <namespace>
  (make-namespace-internal root bindings mutex)
  namespace?
  (root namespace-root)
  (bindings namespace-bindings set-namespace-bindings!)
  (mutex namespace-mutex))

(define-record-type <namespace-entry>
  (make-namespace-entry path type resource metadata)
  namespace-entry?
  (path entry-path)
  (type entry-type)
  (resource entry-resource)
  (metadata entry-metadata set-entry-metadata!))

(define* (make-namespace-manager #:key (root "/"))
  "Create a new namespace manager with Plan9-style hierarchical structure."
  (make-namespace-internal
   root
   (make-hash-table)
   (make-mutex)))

;;;
;;; Namespace Operations
;;;

(define (namespace-bind ns path resource type metadata)
  "Bind a resource to a path in the namespace (Plan9-style bind operation)."
  (with-mutex (namespace-mutex ns)
    (let ((entry (make-namespace-entry path type resource metadata)))
      (hash-set! (namespace-bindings ns) path entry)
      (format #t "[namespace] Bound ~a -> ~a (~a)~%" path type resource)
      #t)))

(define (namespace-unbind ns path)
  "Unbind a resource from the namespace."
  (with-mutex (namespace-mutex ns)
    (hash-remove! (namespace-bindings ns) path)
    (format #t "[namespace] Unbound ~a~%" path)
    #t))

(define (namespace-lookup ns path)
  "Look up a resource by path in the namespace."
  (with-mutex (namespace-mutex ns)
    (hash-ref (namespace-bindings ns) path #f)))

(define (namespace-mount ns source-path target-path)
  "Mount one namespace path onto another (Plan9-style mount)."
  (with-mutex (namespace-mutex ns)
    (let ((source-entry (hash-ref (namespace-bindings ns) source-path)))
      (when source-entry
        (hash-set! (namespace-bindings ns) target-path source-entry)
        (format #t "[namespace] Mounted ~a -> ~a~%" source-path target-path)
        #t))))

(define (namespace-unmount ns path)
  "Unmount a namespace path."
  (namespace-unbind ns path))

(define (namespace-walk ns prefix)
  "Walk the namespace tree, returning all entries under prefix."
  (with-mutex (namespace-mutex ns)
    (hash-fold
     (lambda (path entry acc)
       (if (string-prefix? prefix path)
           (cons (cons path entry) acc)
           acc))
     '()
     (namespace-bindings ns))))

;;;
;;; 9P-Style Service Protocol
;;;
;;; Implements a simplified 9P-inspired protocol for inter-agent
;;; communication and resource sharing.
;;;

(define-record-type <9p-service>
  (make-9p-service-internal namespace port handlers running? mutex)
  9p-service?
  (namespace 9p-namespace)
  (port 9p-port)
  (handlers 9p-handlers set-9p-handlers!)
  (running? 9p-running? set-9p-running?!)
  (mutex 9p-mutex))

(define* (make-9p-service namespace #:key (port DEFAULT-9P-PORT))
  "Create a 9P-style service for namespace access."
  (make-9p-service-internal
   namespace
   port
   (make-hash-table)
   #f
   (make-mutex)))

(define (9p-register-handler service op handler)
  "Register a handler for a 9P-style operation."
  (with-mutex (9p-mutex service)
    (hash-set! (9p-handlers service) op handler)))

(define (9p-service-start service)
  "Start the 9P service daemon."
  (with-mutex (9p-mutex service)
    (unless (9p-running? service)
      (set-9p-running?! service #t)
      (call-with-new-thread
       (lambda ()
         (format #t "[9P] Service started on port ~a~%" (9p-port service))
         (let loop ()
           (when (9p-running? service)
             ;; Process 9P requests
             ;; In a full implementation, this would handle network I/O
             ;; For now, it's a placeholder for the service loop
             (sleep 5)
             (loop)))))
      (format #t "[9P] Service daemon initialized~%")
      #t)))

(define (9p-service-stop service)
  "Stop the 9P service daemon."
  (with-mutex (9p-mutex service)
    (set-9p-running?! service #f)
    (format #t "[9P] Service stopped~%")
    #t))

;;;
;;; Pre-defined Namespace Paths
;;;
;;; Standard paths following Plan9 conventions:
;;; /agents       - Running cognitive agents
;;; /atomspace    - AtomSpace instances
;;; /services     - System services
;;; /proc         - Agent process information
;;; /dev          - Device-like interfaces
;;; /net          - Network resources
;;;
