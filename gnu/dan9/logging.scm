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

(define-module (gnu dan9 logging)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-logging-daemon
            logging-daemon-loop
            log-message
            log-debug
            log-info
            log-warning
            log-error
            log-critical
            make-log-entry
            log-entry?
            log-entry-level
            log-entry-message
            log-entry-source
            log-entry-timestamp))

;;;
;;; Dan9 Logging Module
;;;
;;; Provides centralized logging for daemons.
;;; All daemons can send log messages to the logging daemon.
;;;

;;; Log levels

(define *log-levels*
  '((debug . 0)
    (info . 1)
    (warning . 2)
    (error . 3)
    (critical . 4)))

(define (log-level-value level)
  "Get numeric value for log level."
  (or (assoc-ref *log-levels* level) 0))

;;; Log entry record

(define-record-type <log-entry>
  (make-log-entry level message source timestamp)
  log-entry?
  (level log-entry-level)
  (message log-entry-message)
  (source log-entry-source)
  (timestamp log-entry-timestamp))

;;; Log storage

(define *log-buffer* '())
(define *log-buffer-mutex* (make-mutex))
(define *log-buffer-max-size* 1000)

(define (add-log-entry! entry)
  "Add a log entry to the buffer."
  (with-mutex *log-buffer-mutex*
    (set! *log-buffer* (cons entry *log-buffer*))
    (when (> (length *log-buffer*) *log-buffer-max-size*)
      (set! *log-buffer* (take *log-buffer* *log-buffer-max-size*)))))

(define* (get-log-entries #:key (level #f) (source #f) (limit #f))
  "Get log entries, optionally filtered by level or source."
  (with-mutex *log-buffer-mutex*
    (let* ((entries *log-buffer*)
           (filtered (if level
                        (filter (lambda (e)
                                 (>= (log-level-value (log-entry-level e))
                                     (log-level-value level)))
                               entries)
                        entries))
           (by-source (if source
                         (filter (lambda (e)
                                  (equal? (log-entry-source e) source))
                                filtered)
                         filtered))
           (limited (if limit
                       (take by-source (min limit (length by-source)))
                       by-source)))
      limited)))

;;;
;;; Logging Functions
;;;

(define* (log-message level source message #:key (daemon-name "logging"))
  "Send a log message to the logging daemon."
  (let ((logger (daemon-lookup daemon-name)))
    (when logger
      (let ((entry (make-log-entry level message source
                                  (current-time time-monotonic))))
        (daemon-send-message logger logger 'log
                           `((entry . ,entry)))))))

(define (log-debug source message)
  "Log a debug message."
  (log-message 'debug source message))

(define (log-info source message)
  "Log an info message."
  (log-message 'info source message))

(define (log-warning source message)
  "Log a warning message."
  (log-message 'warning source message))

(define (log-error source message)
  "Log an error message."
  (log-message 'error source message))

(define (log-critical source message)
  "Log a critical message."
  (log-message 'critical source message))

;;;
;;; Log Formatting
;;;

(define (format-log-entry entry)
  "Format a log entry for display."
  (let* ((timestamp (log-entry-timestamp entry))
         (time-str (time-second timestamp))
         (level (log-entry-level entry))
         (source (log-entry-source entry))
         (message (log-entry-message entry)))
    (format #f "[~a] ~8a ~15a: ~a"
            time-str
            (symbol->string level)
            source
            message)))

(define (display-log-entry entry)
  "Display a single log entry."
  (format #t "~a~%" (format-log-entry entry)))

(define* (display-logs #:key (level #f) (source #f) (limit 20))
  "Display recent log entries."
  (let ((entries (get-log-entries #:level level #:source source #:limit limit)))
    (format #t "~%=== Recent Logs (showing ~a) ===~%" (length entries))
    (for-each display-log-entry entries)
    (format #t "=== End Logs ===~%~%")))

;;;
;;; Log File Writing
;;;

(define* (write-logs-to-file filepath #:key (level #f) (source #f))
  "Write log entries to a file."
  (catch #t
    (lambda ()
      (call-with-output-file filepath
        (lambda (port)
          (let ((entries (get-log-entries #:level level #:source source)))
            (for-each
             (lambda (entry)
               (display (format-log-entry entry) port)
               (newline port))
             entries))))
      (format #t "[Logging] Wrote ~a entries to ~a~%"
              (length (get-log-entries #:level level #:source source))
              filepath)
      #t)
    (lambda (key . args)
      (format #t "[Logging] Error writing to ~a: ~a ~a~%"
              filepath key args)
      #f)))

;;;
;;; Logging Daemon
;;;

(define* (make-logging-daemon #:key
                             (name "logging")
                             (min-level 'debug)
                             (log-file #f))
  "Create a logging daemon for centralized logging."
  (make-daemon name 'logging
               #:config `((min-level . ,min-level)
                         (log-file . ,log-file))
               #:auto-register #t))

(define (logging-daemon-loop daemon)
  "Main loop for logging daemon."
  (let ((min-level (or (assoc-ref (daemon-config daemon) 'min-level) 'debug))
        (log-file (assoc-ref (daemon-config daemon) 'log-file)))
    (format #t "[~a] Logging daemon started (min-level: ~a)~%"
            (daemon-name daemon) min-level)
    
    (let loop ()
      (when (eq? 'running (daemon-state daemon))
        ;; Process messages
        (let ((msg (daemon-receive-message daemon)))
          (when msg
            (handle-logging-message daemon msg min-level log-file)))
        
        (usleep 10000) ; 10ms
        (loop)))))

(define (handle-logging-message daemon msg min-level log-file)
  "Handle incoming messages for logging daemon."
  (match (message-type msg)
    ('log
     (let ((entry (assoc-ref (message-payload msg) 'entry)))
       (when (and entry
                 (>= (log-level-value (log-entry-level entry))
                     (log-level-value min-level)))
         (add-log-entry! entry)
         (display-log-entry entry)
         (daemon-set-metric! daemon 'logs-received
                           (+ 1 (or (daemon-get-metric daemon 'logs-received) 0)))
         
         ;; Write to file if configured
         (when log-file
           (catch #t
             (lambda ()
               (call-with-output-file log-file
                 (lambda (port)
                   (display (format-log-entry entry) port)
                   (newline port))
                 #:binary #f
                 #:encoding "UTF-8"))
             (lambda (key . args)
               (format #t "[~a] Error writing to log file: ~a ~a~%"
                       (daemon-name daemon) key args)))))))
    
    ('display
     (let ((level (assoc-ref (message-payload msg) 'level))
           (source (assoc-ref (message-payload msg) 'source))
           (limit (or (assoc-ref (message-payload msg) 'limit) 20)))
       (display-logs #:level level #:source source #:limit limit)))
    
    ('write-file
     (let ((filepath (assoc-ref (message-payload msg) 'filepath))
           (level (assoc-ref (message-payload msg) 'level))
           (source (assoc-ref (message-payload msg) 'source)))
       (when filepath
         (write-logs-to-file filepath #:level level #:source source))))
    
    ('clear
     (with-mutex *log-buffer-mutex*
       (set! *log-buffer* '())
       (format #t "[~a] Log buffer cleared~%" (daemon-name daemon))))
    
    ('stats
     (with-mutex *log-buffer-mutex*
       (format #t "[~a] Stats: ~a entries in buffer~%"
               (daemon-name daemon) (length *log-buffer*))))
    
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type msg)))))
