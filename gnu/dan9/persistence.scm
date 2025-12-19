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

(define-module (gnu dan9 persistence)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-persistence-daemon
            persistence-daemon-loop
            daemon-save-state
            daemon-load-state
            daemon-checkpoint
            daemon-restore
            make-checkpoint-policy
            checkpoint-policy?
            checkpoint-policy-interval
            checkpoint-policy-on-stop
            checkpoint-policy-on-change))

;;;
;;; Dan9 Persistence Module
;;;
;;; Provides state persistence and recovery for daemons.
;;; Allows daemons to save their state to disk and recover from failures.
;;;

;;; Checkpoint policy record

(define-record-type <checkpoint-policy>
  (make-checkpoint-policy interval on-stop on-change)
  checkpoint-policy?
  (interval checkpoint-policy-interval)      ; Checkpoint every N seconds
  (on-stop checkpoint-policy-on-stop)        ; Checkpoint when daemon stops
  (on-change checkpoint-policy-on-change))   ; Checkpoint on state changes

;;; Default checkpoint policy

(define *default-checkpoint-policy*
  (make-checkpoint-policy 300 #t #f)) ; Every 5 minutes, on stop, not on change

;;; State serialization

(define (serialize-daemon-state daemon)
  "Serialize daemon state to an association list."
  (list (cons 'name (daemon-name daemon))
        (cons 'type (daemon-type daemon))
        (cons 'state (daemon-state daemon))
        (cons 'metrics (daemon-get-metrics daemon))
        (cons 'config (daemon-config daemon))
        (cons 'timestamp (time-second (current-time time-monotonic)))))

(define (write-state-to-file state filepath)
  "Write serialized state to file."
  (catch #t
    (lambda ()
      (call-with-output-file filepath
        (lambda (port)
          (pretty-print state port)))
      #t)
    (lambda (key . args)
      (format #t "[Persistence] Error writing state to ~a: ~a ~a~%"
              filepath key args)
      #f)))

(define (read-state-from-file filepath)
  "Read serialized state from file."
  (catch #t
    (lambda ()
      (call-with-input-file filepath
        (lambda (port)
          (read port))))
    (lambda (key . args)
      (format #t "[Persistence] Error reading state from ~a: ~a ~a~%"
              filepath key args)
      #f)))

;;;
;;; Public API
;;;

(define* (daemon-save-state daemon #:key (directory "/tmp/dan9-state"))
  "Save daemon state to disk."
  (let* ((state (serialize-daemon-state daemon))
         (filename (string-append (daemon-name daemon) ".state"))
         (filepath (string-append directory "/" filename)))
    ;; Ensure directory exists
    (catch #t
      (lambda ()
        (mkdir directory))
      (lambda (key . args)
        #f)) ; Directory might already exist
    ;; Write state
    (if (write-state-to-file state filepath)
        (begin
          (format #t "[~a] State saved to ~a~%" (daemon-name daemon) filepath)
          filepath)
        #f)))

(define* (daemon-load-state daemon #:key (directory "/tmp/dan9-state"))
  "Load daemon state from disk."
  (let* ((filename (string-append (daemon-name daemon) ".state"))
         (filepath (string-append directory "/" filename)))
    (let ((state (read-state-from-file filepath)))
      (when state
        (format #t "[~a] State loaded from ~a~%" (daemon-name daemon) filepath)
        ;; Note: Metrics would need to be restored via daemon-set-metric!
        ;; This returns the state for the caller to apply
        state))))

(define* (daemon-checkpoint daemon #:key
                           (directory "/tmp/dan9-state")
                           (policy *default-checkpoint-policy*))
  "Create a checkpoint of daemon state."
  (daemon-save-state daemon #:directory directory))

(define* (daemon-restore daemon #:key (directory "/tmp/dan9-state"))
  "Restore daemon from checkpoint."
  (daemon-load-state daemon #:directory directory))

;;;
;;; Persistence Daemon
;;;

(define* (make-persistence-daemon #:key
                                 (name "persistence")
                                 (state-directory "/tmp/dan9-state")
                                 (checkpoint-interval 300))
  "Create a persistence daemon that manages state checkpoints."
  (make-daemon name 'persistence
               #:config `((state-directory . ,state-directory)
                         (checkpoint-interval . ,checkpoint-interval))
               #:auto-register #t))

(define (persistence-daemon-loop daemon)
  "Main loop for persistence daemon."
  (let ((state-dir (or (assoc-ref (daemon-config daemon) 'state-directory)
                      "/tmp/dan9-state"))
        (interval (or (assoc-ref (daemon-config daemon) 'checkpoint-interval)
                     300)))
    (format #t "[~a] Persistence daemon started (dir: ~a, interval: ~a)~%"
            (daemon-name daemon) state-dir interval)
    
    (let loop ((last-checkpoint (current-time time-monotonic)))
      (when (eq? 'running (daemon-state daemon))
        ;; Process messages
        (let ((msg (daemon-receive-message daemon)))
          (when msg
            (handle-persistence-message daemon msg state-dir)))
        
        ;; Periodic checkpoint
        (let* ((now (current-time time-monotonic))
               (elapsed (- (time-second now) (time-second last-checkpoint))))
          (if (>= elapsed interval)
              (begin
                (checkpoint-all-daemons daemon state-dir)
                (usleep 100000) ; 100ms
                (loop now))
              (begin
                (usleep 100000) ; 100ms
                (loop last-checkpoint))))))))

(define (handle-persistence-message daemon msg state-dir)
  "Handle incoming messages for persistence daemon."
  (match (message-type msg)
    ('save
     (let ((target-name (assoc-ref (message-payload msg) 'daemon-name)))
       (when target-name
         (let ((target (daemon-lookup target-name)))
           (when target
             (daemon-save-state target #:directory state-dir)
             (format #t "[~a] Saved state for daemon: ~a~%"
                     (daemon-name daemon) target-name))))))
    
    ('load
     (let ((target-name (assoc-ref (message-payload msg) 'daemon-name)))
       (when target-name
         (let ((target (daemon-lookup target-name)))
           (when target
             (daemon-load-state target #:directory state-dir)
             (format #t "[~a] Loaded state for daemon: ~a~%"
                     (daemon-name daemon) target-name))))))
    
    ('checkpoint-all
     (checkpoint-all-daemons daemon state-dir))
    
    ('list
     (list-checkpoints daemon state-dir))
    
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type msg)))))

(define (checkpoint-all-daemons persistence-daemon state-dir)
  "Checkpoint all registered daemons."
  (let ((daemons (daemon-list-all)))
    (format #t "[~a] Checkpointing ~a daemons~%"
            (daemon-name persistence-daemon) (length daemons))
    (for-each
     (lambda (daemon-name)
       (let ((daemon (daemon-lookup daemon-name)))
         (when (and daemon
                   (not (equal? daemon-name (daemon-name persistence-daemon))))
           (daemon-save-state daemon #:directory state-dir))))
     daemons)))

(define (list-checkpoints persistence-daemon state-dir)
  "List all available checkpoints."
  (catch #t
    (lambda ()
      (let ((files (scandir state-dir
                           (lambda (f)
                             (string-suffix? ".state" f)))))
        (format #t "[~a] Available checkpoints: ~a~%"
                (daemon-name persistence-daemon)
                (length files))
        (for-each
         (lambda (f)
           (format #t "  - ~a~%" f))
         files)))
    (lambda (key . args)
      (format #t "[~a] Error listing checkpoints: ~a ~a~%"
              (daemon-name persistence-daemon) key args))))
