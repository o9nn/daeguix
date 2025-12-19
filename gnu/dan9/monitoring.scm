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

(define-module (gnu dan9 monitoring)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-monitoring-daemon
            monitoring-daemon-loop
            monitor-snapshot
            monitor-history
            monitor-aggregate
            display-dashboard
            display-daemon-status
            display-metrics-table
            make-metric-history
            metric-history?
            metric-history-values
            metric-history-add!))

;;;
;;; Dan9 Monitoring Module
;;;
;;; Provides real-time monitoring and dashboard for daemons.
;;; Collects metrics, maintains history, and displays status.
;;;

;;; Metric history record

(define-record-type <metric-history>
  (make-metric-history-internal max-size values mutex)
  metric-history?
  (max-size metric-history-max-size)
  (values metric-history-values set-metric-history-values!)
  (mutex metric-history-mutex))

(define* (make-metric-history #:key (max-size 100))
  "Create a metric history buffer with maximum size."
  (make-metric-history-internal max-size '() (make-mutex)))

(define (metric-history-add! history timestamp value)
  "Add a metric value to history with timestamp."
  (with-mutex (metric-history-mutex history)
    (let* ((current (metric-history-values history))
           (new-entry (cons timestamp value))
           (updated (cons new-entry current))
           (trimmed (if (> (length updated) (metric-history-max-size history))
                       (take updated (metric-history-max-size history))
                       updated)))
      (set-metric-history-values! history trimmed))))

;;; Monitoring state

(define *monitoring-state* (make-hash-table))
(define *monitoring-mutex* (make-mutex))

(define (get-daemon-history daemon-name)
  "Get or create metric history for a daemon."
  (with-mutex *monitoring-mutex*
    (or (hash-ref *monitoring-state* daemon-name #f)
        (let ((history (make-hash-table)))
          (hash-set! *monitoring-state* daemon-name history)
          history))))

;;;
;;; Monitoring Functions
;;;

(define (monitor-snapshot)
  "Take a snapshot of all daemon states."
  (let ((daemons (daemon-list-all))
        (timestamp (current-time time-monotonic)))
    (map (lambda (daemon-name)
           (let ((daemon (daemon-lookup daemon-name)))
             (when daemon
               (cons daemon-name
                     (list (cons 'status (daemon-status daemon))
                           (cons 'metrics (daemon-get-metrics daemon))
                           (cons 'timestamp (time-second timestamp)))))))
         daemons)))

(define (monitor-collect-metrics daemon-name)
  "Collect and store metrics for a daemon."
  (let* ((daemon (daemon-lookup daemon-name))
         (timestamp (time-second (current-time time-monotonic))))
    (when daemon
      (let ((metrics (daemon-get-metrics daemon))
            (history (get-daemon-history daemon-name)))
        (for-each
         (lambda (metric-pair)
           (let* ((key (car metric-pair))
                  (value (cdr metric-pair))
                  (metric-hist (or (hash-ref history key #f)
                                  (let ((h (make-metric-history)))
                                    (hash-set! history key h)
                                    h))))
             (metric-history-add! metric-hist timestamp value)))
         metrics)))))

(define* (monitor-history daemon-name #:key (metric-name #f))
  "Get metric history for a daemon."
  (let ((history (get-daemon-history daemon-name)))
    (if metric-name
        (let ((metric-hist (hash-ref history metric-name #f)))
          (if metric-hist
              (metric-history-values metric-hist)
              '()))
        (hash-map->list (lambda (k v)
                         (cons k (metric-history-values v)))
                       history))))

(define (monitor-aggregate daemon-name metric-name)
  "Compute aggregate statistics for a metric."
  (let ((values (map cdr (monitor-history daemon-name
                                         #:metric-name metric-name))))
    (if (null? values)
        '()
        (let* ((numeric-values (filter number? values))
               (count (length numeric-values))
               (sum (apply + numeric-values))
               (avg (if (> count 0) (/ sum count) 0))
               (min-val (if (null? numeric-values) 0 (apply min numeric-values)))
               (max-val (if (null? numeric-values) 0 (apply max numeric-values))))
          (list (cons 'count count)
                (cons 'sum sum)
                (cons 'average avg)
                (cons 'min min-val)
                (cons 'max max-val))))))

;;;
;;; Dashboard Display
;;;

(define (display-dashboard)
  "Display a text-based dashboard of all daemons."
  (format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║              DAN9 DAEMON MONITORING DASHBOARD                 ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
  
  (let ((daemons (daemon-list-all)))
    (format #t "Active Daemons: ~a~%~%" (length daemons))
    
    (for-each
     (lambda (daemon-name)
       (let ((daemon (daemon-lookup daemon-name)))
         (when daemon
           (display-daemon-status daemon))))
     daemons)))

(define (display-daemon-status daemon)
  "Display status of a single daemon."
  (let ((status (daemon-status daemon))
        (metrics (daemon-get-metrics daemon)))
    (format #t "┌─ ~a (~a)~%" 
            (daemon-name daemon) (daemon-type daemon))
    (format #t "│  State: ~a~%" (assoc-ref status 'state))
    (format #t "│  Thread: ~a~%" 
            (if (assoc-ref status 'thread-alive?) "alive" "dead"))
    
    (when (not (null? metrics))
      (format #t "│  Metrics:~%")
      (for-each
       (lambda (metric)
         (format #t "│    - ~a: ~a~%" (car metric) (cdr metric)))
       metrics))
    
    (format #t "└─~%~%")))

(define (display-metrics-table daemon-names metric-name)
  "Display a table of metric values across daemons."
  (format #t "~%Metric: ~a~%" metric-name)
  (format #t "┌────────────────────┬──────────┬──────────┬──────────┐~%")
  (format #t "│ Daemon             │ Current  │ Min      │ Max      │~%")
  (format #t "├────────────────────┼──────────┼──────────┼──────────┤~%")
  
  (for-each
   (lambda (daemon-name)
     (let ((daemon (daemon-lookup daemon-name)))
       (when daemon
         (let* ((current (daemon-get-metric daemon metric-name))
                (agg (monitor-aggregate daemon-name metric-name))
                (min-val (assoc-ref agg 'min))
                (max-val (assoc-ref agg 'max)))
           (format #t "│ ~18a │ ~8a │ ~8a │ ~8a │~%"
                   (string-take daemon-name (min 18 (string-length daemon-name)))
                   (or current "-")
                   (or min-val "-")
                   (or max-val "-"))))))
   daemon-names)
  
  (format #t "└────────────────────┴──────────┴──────────┴──────────┘~%~%"))

;;;
;;; Monitoring Daemon
;;;

(define* (make-monitoring-daemon #:key
                                (name "monitoring")
                                (collect-interval 5))
  "Create a monitoring daemon for metrics collection."
  (make-daemon name 'monitoring
               #:config `((collect-interval . ,collect-interval))
               #:auto-register #t))

(define (monitoring-daemon-loop daemon)
  "Main loop for monitoring daemon."
  (let ((interval (or (assoc-ref (daemon-config daemon) 'collect-interval) 5)))
    (format #t "[~a] Monitoring daemon started (interval: ~as)~%"
            (daemon-name daemon) interval)
    
    (let loop ((last-collect (current-time time-monotonic)))
      (when (eq? 'running (daemon-state daemon))
        ;; Process messages
        (let ((msg (daemon-receive-message daemon)))
          (when msg
            (handle-monitoring-message daemon msg)))
        
        ;; Periodic collection
        (let* ((now (current-time time-monotonic))
               (elapsed (- (time-second now) (time-second last-collect))))
          (if (>= elapsed interval)
              (begin
                (collect-all-metrics daemon)
                (usleep 100000) ; 100ms
                (loop now))
              (begin
                (usleep 100000) ; 100ms
                (loop last-collect))))))))

(define (handle-monitoring-message daemon msg)
  "Handle incoming messages for monitoring daemon."
  (match (message-type msg)
    ('snapshot
     (let ((snap (monitor-snapshot)))
       (format #t "[~a] Snapshot taken: ~a daemons~%"
               (daemon-name daemon) (length snap))))
    
    ('dashboard
     (display-dashboard))
    
    ('status
     (let ((target-name (assoc-ref (message-payload msg) 'daemon-name)))
       (when target-name
         (let ((target (daemon-lookup target-name)))
           (when target
             (display-daemon-status target))))))
    
    ('history
     (let ((target-name (assoc-ref (message-payload msg) 'daemon-name))
           (metric-name (assoc-ref (message-payload msg) 'metric-name)))
       (when target-name
         (let ((history (monitor-history target-name
                                        #:metric-name metric-name)))
           (format #t "[~a] History for ~a/~a: ~a entries~%"
                   (daemon-name daemon) target-name metric-name
                   (length history))))))
    
    ('aggregate
     (let ((target-name (assoc-ref (message-payload msg) 'daemon-name))
           (metric-name (assoc-ref (message-payload msg) 'metric-name)))
       (when (and target-name metric-name)
         (let ((agg (monitor-aggregate target-name metric-name)))
           (format #t "[~a] Aggregate for ~a/~a:~%" 
                   (daemon-name daemon) target-name metric-name)
           (for-each
            (lambda (stat)
              (format #t "  ~a: ~a~%" (car stat) (cdr stat)))
            agg)))))
    
    ('metrics-table
     (let ((metric-name (assoc-ref (message-payload msg) 'metric-name)))
       (when metric-name
         (display-metrics-table (daemon-list-all) metric-name))))
    
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type msg)))))

(define (collect-all-metrics monitoring-daemon)
  "Collect metrics from all registered daemons."
  (let ((daemons (daemon-list-all)))
    (for-each
     (lambda (daemon-name)
       (when (not (equal? daemon-name (daemon-name monitoring-daemon)))
         (monitor-collect-metrics daemon-name)))
     daemons)
    (daemon-set-metric! monitoring-daemon 'collections
                       (+ 1 (or (daemon-get-metric monitoring-daemon 'collections) 0)))))
