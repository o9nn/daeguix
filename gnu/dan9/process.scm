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

(define-module (gnu dan9 process)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (make-process-daemon
            process-daemon-loop))

;;;
;;; Process Management Daemon
;;;
;;; In dan9, process management is handled by a daemon. Instead of direct
;;; fork/exec calls, clients send messages to the process daemon to create,
;;; manage, and monitor processes.
;;;

(define* (make-process-daemon #:key
                             (name "process")
                             (max-processes 1000))
  "Create a process management daemon."
  (make-daemon name 'process
               #:config (list (cons 'max-processes max-processes)
                            (cons 'process-table (make-hash-table)))
               #:loop-fn process-daemon-loop))

(define (process-daemon-loop daemon)
  "Main loop for the process management daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Check for incoming messages
      (let ((message (daemon-receive-message daemon)))
        (when message
          (handle-process-message daemon message)))
      
      ;; Update process states
      (update-process-states daemon)
      
      ;; Update metrics
      (daemon-set-metric! daemon 'last-poll (current-time time-monotonic))
      
      ;; Sleep briefly
      (usleep 10000) ; 10ms
      (loop))))

(define (handle-process-message daemon message)
  "Handle a process management message."
  (match (message-type message)
    ('spawn (handle-spawn daemon message))
    ('kill (handle-kill daemon message))
    ('wait (handle-wait daemon message))
    ('list (handle-list-processes daemon message))
    ('getpid (handle-getpid daemon message))
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type message)))))

(define (handle-spawn daemon message)
  "Handle a process spawn request."
  (let* ((payload (message-payload message))
         (command (assoc-ref payload 'command))
         (args (assoc-ref payload 'args))
         (process-table (assoc-ref (daemon-config daemon) 'process-table))
         (pid (+ 1000 (hash-count (const #t) process-table))))
    (format #t "[~a] Spawn request: ~a ~a (pid: ~a)~%"
            (daemon-name daemon) command args pid)
    
    ;; Store process info in process table
    (hash-set! process-table pid
               (list (cons 'command command)
                     (cons 'args args)
                     (cons 'state 'running)
                     (cons 'start-time (current-time time-monotonic))))
    
    (daemon-set-metric! daemon 'spawn-count
                       (+ 1 (or (daemon-get-metric daemon 'spawn-count) 0)))))

(define (handle-kill daemon message)
  "Handle a process kill request."
  (let* ((payload (message-payload message))
         (pid (assoc-ref payload 'pid))
         (signal (assoc-ref payload 'signal)))
    (format #t "[~a] Kill request: pid ~a signal ~a~%"
            (daemon-name daemon) pid signal)
    (daemon-set-metric! daemon 'kill-count
                       (+ 1 (or (daemon-get-metric daemon 'kill-count) 0)))))

(define (handle-wait daemon message)
  "Handle a process wait request."
  (let* ((payload (message-payload message))
         (pid (assoc-ref payload 'pid)))
    (format #t "[~a] Wait request: pid ~a~%"
            (daemon-name daemon) pid)
    (daemon-set-metric! daemon 'wait-count
                       (+ 1 (or (daemon-get-metric daemon 'wait-count) 0)))))

(define (handle-list-processes daemon message)
  "Handle a process list request."
  (let ((process-table (assoc-ref (daemon-config daemon) 'process-table)))
    (format #t "[~a] List processes request (~a processes)~%"
            (daemon-name daemon)
            (hash-count (const #t) process-table))
    (daemon-set-metric! daemon 'list-count
                       (+ 1 (or (daemon-get-metric daemon 'list-count) 0)))))

(define (handle-getpid daemon message)
  "Handle a getpid request."
  (format #t "[~a] Getpid request~%" (daemon-name daemon))
  (daemon-set-metric! daemon 'getpid-count
                     (+ 1 (or (daemon-get-metric daemon 'getpid-count) 0))))

(define (update-process-states daemon)
  "Update the states of managed processes."
  ;; In a real implementation, this would check process states
  ;; For now, it's a placeholder
  #t)
