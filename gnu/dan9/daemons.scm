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

(define-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-daemon
            daemon?
            daemon-name
            daemon-type
            daemon-state
            daemon-start
            daemon-stop
            daemon-status
            daemon-send-message
            daemon-receive-message
            daemon-get-metrics
            daemon-register
            daemon-lookup
            daemon-list-all))

;;;
;;; Dan9 Core Daemon Infrastructure
;;;
;;; In dan9, everything is a daemon. This module provides the core primitives
;;; for creating, managing, and communicating with daemons.
;;;
;;; Like plan9's "everything is a file" philosophy, dan9's "everything is a
;;; daemon" means that all system resources and services are accessed through
;;; daemon interfaces with message passing.
;;;

;;; Record type for daemons

(define-record-type <daemon>
  (make-daemon-internal name type state thread mutex config metrics mailbox registry-ref)
  daemon?
  (name daemon-name)
  (type daemon-type)
  (state daemon-state set-daemon-state!)
  (thread daemon-thread set-daemon-thread!)
  (mutex daemon-mutex)
  (config daemon-config)
  (metrics daemon-metrics set-daemon-metrics!)
  (mailbox daemon-mailbox)
  (registry-ref daemon-registry-ref set-daemon-registry-ref!))

;;; Global daemon registry

(define *daemon-registry* (make-hash-table))
(define *daemon-registry-mutex* (make-mutex))

;;; Message type for inter-daemon communication

(define-record-type <daemon-message>
  (make-message sender recipient type payload timestamp)
  message?
  (sender message-sender)
  (recipient message-recipient)
  (type message-type)
  (payload message-payload)
  (timestamp message-timestamp))

;;;
;;; Daemon Creation and Lifecycle
;;;

(define* (make-daemon name type #:key
                     (loop-fn #f)
                     (config '())
                     (auto-register #t))
  "Create a new daemon with NAME and TYPE.
LOOP-FN is the main loop function for the daemon.
CONFIG is an association list of configuration parameters.
If AUTO-REGISTER is true, register the daemon in the global registry."
  (let ((daemon (make-daemon-internal
                 name
                 type
                 'stopped
                 #f
                 (make-mutex)
                 config
                 (make-hash-table)
                 (make-mailbox)
                 #f)))
    (when auto-register
      (daemon-register daemon))
    daemon))

(define (make-mailbox)
  "Create a mailbox for daemon message passing."
  (list (make-mutex) '()))

(define (mailbox-send! mailbox message)
  "Send a message to a mailbox."
  (let ((mutex (car mailbox)))
    (with-mutex mutex
      (set-cdr! mailbox (append (cdr mailbox) (list message))))))

(define (mailbox-receive! mailbox)
  "Receive a message from a mailbox. Returns #f if no messages."
  (let ((mutex (car mailbox)))
    (with-mutex mutex
      (if (null? (cdr mailbox))
          #f
          (let ((message (car (cdr mailbox))))
            (set-cdr! mailbox (cdr (cdr mailbox)))
            message)))))

;;;
;;; Daemon Control Operations
;;;

(define (daemon-start daemon loop-fn)
  "Start a daemon with the given loop function."
  (with-mutex (daemon-mutex daemon)
    (unless (eq? 'running (daemon-state daemon))
      (set-daemon-state! daemon 'running)
      (let ((thread
             (call-with-new-thread
              (lambda ()
                (catch #t
                  (lambda ()
                    (loop-fn daemon))
                  (lambda (key . args)
                    (format #t "[~a] Error in daemon loop: ~a ~a~%"
                            (daemon-name daemon) key args)
                    (set-daemon-state! daemon 'error)))))))
        (set-daemon-thread! daemon thread)
        (format #t "[~a] Daemon started (type: ~a)~%" 
                (daemon-name daemon) (daemon-type daemon))
        #t))))

(define (daemon-stop daemon)
  "Stop a daemon."
  (with-mutex (daemon-mutex daemon)
    (when (eq? 'running (daemon-state daemon))
      (set-daemon-state! daemon 'stopped)
      (format #t "[~a] Daemon stopped~%" (daemon-name daemon))
      #t)))

(define (daemon-status daemon)
  "Get daemon status."
  (with-mutex (daemon-mutex daemon)
    (list (cons 'name (daemon-name daemon))
          (cons 'type (daemon-type daemon))
          (cons 'state (daemon-state daemon))
          (cons 'thread-alive? (and (daemon-thread daemon)
                                   (not (thread-exited? (daemon-thread daemon))))))))

;;;
;;; Inter-Daemon Communication
;;;

(define (daemon-send-message sender recipient type payload)
  "Send a message from one daemon to another."
  (let ((message (make-message
                  (daemon-name sender)
                  (daemon-name recipient)
                  type
                  payload
                  (current-time time-monotonic))))
    (mailbox-send! (daemon-mailbox recipient) message)
    message))

(define (daemon-receive-message daemon)
  "Receive a message from daemon's mailbox."
  (mailbox-receive! (daemon-mailbox daemon)))

;;;
;;; Daemon Registry
;;;

(define (daemon-register daemon)
  "Register a daemon in the global registry."
  (with-mutex *daemon-registry-mutex*
    (hash-set! *daemon-registry* (daemon-name daemon) daemon)
    (set-daemon-registry-ref! daemon #t)
    (format #t "[Registry] Registered daemon: ~a~%" (daemon-name daemon))
    #t))

(define (daemon-lookup name)
  "Look up a daemon by name in the registry."
  (with-mutex *daemon-registry-mutex*
    (hash-ref *daemon-registry* name #f)))

(define (daemon-list-all)
  "List all registered daemons."
  (with-mutex *daemon-registry-mutex*
    (hash-map->list (lambda (k v) k) *daemon-registry*)))

;;;
;;; Metrics Collection
;;;

(define (daemon-get-metrics daemon)
  "Get current metrics from daemon."
  (with-mutex (daemon-mutex daemon)
    (hash-map->list cons (daemon-metrics daemon))))

(define (daemon-set-metric! daemon key value)
  "Set a metric value for the daemon."
  (with-mutex (daemon-mutex daemon)
    (hash-set! (daemon-metrics daemon) key value)))

(define (daemon-get-metric daemon key)
  "Get a specific metric value from daemon."
  (with-mutex (daemon-mutex daemon)
    (hash-ref (daemon-metrics daemon) key #f)))
