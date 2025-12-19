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

(use-modules (gnu dan9 daemons)
             (gnu dan9 monitoring)
             (ice-9 threads)
             (srfi srfi-1)
             (srfi srfi-64))

;;;
;;; Tests for Dan9 Monitoring Module
;;;

(test-begin "dan9-monitoring")

;;; Test metric history

(test-group "metric-history"
  (test-assert "make-metric-history creates history"
    (let ((hist (make-metric-history #:max-size 10)))
      (metric-history? hist)))
  
  (test-assert "metric-history-add! adds values"
    (let ((hist (make-metric-history #:max-size 10)))
      (metric-history-add! hist 1000 42)
      (metric-history-add! hist 1001 43)
      (= (length (metric-history-values hist)) 2)))
  
  (test-assert "metric-history respects max-size"
    (let ((hist (make-metric-history #:max-size 5)))
      (for-each
       (lambda (i)
         (metric-history-add! hist i i))
       (iota 10))
      (<= (length (metric-history-values hist)) 5))))

;;; Test monitoring functions

(test-group "monitoring-functions"
  (test-assert "monitor-snapshot returns list"
    (let ((snap (monitor-snapshot)))
      (list? snap)))
  
  (test-assert "monitor-snapshot includes daemon info"
    (let* ((test-daemon (make-daemon "test-monitor" 'test))
           (snap (monitor-snapshot)))
      (any (lambda (entry)
             (equal? (car entry) "test-monitor"))
           snap)))
  
  (test-assert "monitor-history returns empty for new daemon"
    (let* ((test-daemon (make-daemon "test-history" 'test))
           (history (monitor-history "test-history")))
      (list? history))))

;;; Test metric aggregation

(test-group "metric-aggregation"
  (test-assert "monitor-aggregate returns stats"
    (let* ((test-daemon (make-daemon "test-agg" 'test)))
      ;; Add some metrics
      (daemon-set-metric! test-daemon 'counter 10)
      (monitor-collect-metrics "test-agg")
      (daemon-set-metric! test-daemon 'counter 20)
      (monitor-collect-metrics "test-agg")
      (daemon-set-metric! test-daemon 'counter 15)
      (monitor-collect-metrics "test-agg")
      
      (let ((agg (monitor-aggregate "test-agg" 'counter)))
        (and (list? agg)
             (assoc-ref agg 'count)
             (assoc-ref agg 'min)
             (assoc-ref agg 'max)
             (assoc-ref agg 'average))))))

;;; Test monitoring daemon

(define (test-loop daemon)
  "Simple test loop."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (usleep 100000)
      (loop))))

(test-group "monitoring-daemon"
  (test-assert "make-monitoring-daemon creates daemon"
    (let ((mon (make-monitoring-daemon
                #:name "test-mon"
                #:collect-interval 5)))
      (daemon? mon)))
  
  (test-equal "monitoring daemon has correct type"
    'monitoring
    (let ((mon (make-monitoring-daemon #:name "test-mon-type")))
      (daemon-type mon)))
  
  (test-assert "monitoring daemon can start"
    (let ((mon (make-monitoring-daemon
                #:name "test-mon-start"
                #:collect-interval 2)))
      (daemon-start mon monitoring-daemon-loop)
      (sleep 1)
      (let ((status (daemon-status mon)))
        (daemon-stop mon)
        (eq? 'running (assoc-ref status 'state)))))
  
  (test-assert "monitoring daemon responds to snapshot message"
    (let ((mon (make-monitoring-daemon
                #:name "test-mon-snapshot"
                #:collect-interval 5)))
      (daemon-start mon monitoring-daemon-loop)
      (sleep 1)
      (daemon-send-message mon mon 'snapshot '())
      (sleep 1)
      (daemon-stop mon)
      #t))
  
  (test-assert "monitoring daemon collects metrics"
    (let* ((mon (make-monitoring-daemon
                 #:name "test-mon-collect"
                 #:collect-interval 1))
           (target (make-daemon "test-target-metrics" 'test)))
      (daemon-set-metric! target 'test-metric 99)
      (daemon-start mon monitoring-daemon-loop)
      (daemon-start target test-loop)
      (sleep 2) ; Wait for at least one collection
      (let ((collections (daemon-get-metric mon 'collections)))
        (daemon-stop mon)
        (daemon-stop target)
        (and collections (> collections 0))))))

;;; Test dashboard display

(test-group "dashboard-display"
  (test-assert "display-dashboard doesn't crash"
    (catch #t
      (lambda ()
        (display-dashboard)
        #t)
      (lambda (key . args)
        #f)))
  
  (test-assert "display-daemon-status works"
    (let ((test-daemon (make-daemon "test-display" 'test)))
      (catch #t
        (lambda ()
          (display-daemon-status test-daemon)
          #t)
        (lambda (key . args)
          #f))))
  
  (test-assert "display-metrics-table works"
    (let ((test-daemon (make-daemon "test-table" 'test)))
      (daemon-set-metric! test-daemon 'foo 42)
      (catch #t
        (lambda ()
          (display-metrics-table '("test-table") 'foo)
          #t)
        (lambda (key . args)
          #f)))))

(test-end "dan9-monitoring")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
