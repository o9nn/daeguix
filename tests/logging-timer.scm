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
             (gnu dan9 logging)
             (gnu dan9 timer)
             (ice-9 threads)
             (srfi srfi-1)
             (srfi srfi-64))

;;;
;;; Tests for Dan9 Logging and Timer Modules
;;;

(test-begin "dan9-additional-daemons")

;;; Test logging daemon

(test-group "logging-daemon"
  (test-assert "make-logging-daemon creates daemon"
    (let ((logger (make-logging-daemon #:name "test-logger")))
      (daemon? logger)))
  
  (test-equal "logging daemon has correct type"
    'logging
    (let ((logger (make-logging-daemon #:name "test-logger-type")))
      (daemon-type logger)))
  
  (test-assert "logging daemon can start"
    (let ((logger (make-logging-daemon #:name "test-logger-start")))
      (daemon-start logger logging-daemon-loop)
      (sleep 1)
      (let ((status (daemon-status logger)))
        (daemon-stop logger)
        (eq? 'running (assoc-ref status 'state)))))
  
  (test-assert "log-entry record works"
    (let ((entry (make-log-entry 'info "test message" "test-source"
                                (current-time time-monotonic))))
      (log-entry? entry)))
  
  (test-assert "can log messages"
    (let ((logger (make-logging-daemon #:name "test-log-msgs")))
      (daemon-start logger logging-daemon-loop)
      (sleep 1)
      (log-info "test" "Test message")
      (sleep 1)
      (let ((received (daemon-get-metric logger 'logs-received)))
        (daemon-stop logger)
        (and received (> received 0))))))

;;; Test timer daemon

(test-group "timer-daemon"
  (test-assert "make-timer-daemon creates daemon"
    (let ((timer (make-timer-daemon #:name "test-timer")))
      (daemon? timer)))
  
  (test-equal "timer daemon has correct type"
    'timer
    (let ((timer (make-timer-daemon #:name "test-timer-type")))
      (daemon-type timer)))
  
  (test-assert "timer daemon can start"
    (let ((timer (make-timer-daemon #:name "test-timer-start")))
      (daemon-start timer timer-daemon-loop)
      (sleep 1)
      (let ((status (daemon-status timer)))
        (daemon-stop timer)
        (eq? 'running (assoc-ref status 'state)))))
  
  (test-assert "scheduled-task record works"
    (let ((task (make-scheduled-task 10 "target" 'test '())))
      (scheduled-task? task)))
  
  (test-assert "can schedule tasks"
    (let ((timer (make-timer-daemon #:name "test-schedule")))
      (daemon-start timer timer-daemon-loop)
      (sleep 1)
      (schedule-task 1 "target" 'test '() #:daemon-name "test-schedule")
      (sleep 1)
      (let ((scheduled (daemon-get-metric timer 'tasks-scheduled)))
        (daemon-stop timer)
        (and scheduled (> scheduled 0))))))

;;; Test task execution

(define (test-target-loop daemon)
  "Simple test target loop."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (daemon-set-metric! daemon 'messages-received
                            (+ 1 (or (daemon-get-metric daemon 'messages-received) 0)))))
      (usleep 100000)
      (loop))))

(test-group "task-execution"
  (test-assert "scheduled task fires"
    (let* ((timer (make-timer-daemon #:name "test-fire" #:tick-interval 1))
           (target (make-daemon "test-target" 'test)))
      (daemon-start timer timer-daemon-loop)
      (daemon-start target test-target-loop)
      (sleep 1)
      (schedule-task 2 "test-target" 'test-message '()
                    #:daemon-name "test-fire")
      (sleep 3)
      (let ((fired (daemon-get-metric timer 'tasks-fired))
            (received (daemon-get-metric target 'messages-received)))
        (daemon-stop timer)
        (daemon-stop target)
        (and fired (> fired 0) received (> received 0)))))
  
  (test-assert "repeating task fires multiple times"
    (let* ((timer (make-timer-daemon #:name "test-repeat" #:tick-interval 1))
           (target (make-daemon "test-target-repeat" 'test)))
      (daemon-start timer timer-daemon-loop)
      (daemon-start target test-target-loop)
      (sleep 1)
      (schedule-repeating-task 1 "test-target-repeat" 'test-message '()
                              #:daemon-name "test-repeat")
      (sleep 4)
      (let ((fired (daemon-get-metric timer 'tasks-fired)))
        (daemon-stop timer)
        (daemon-stop target)
        (and fired (>= fired 2))))))

;;; Test task cancellation

(test-group "task-cancellation"
  (test-assert "can cancel scheduled task"
    (let ((timer (make-timer-daemon #:name "test-cancel")))
      (daemon-start timer timer-daemon-loop)
      (sleep 1)
      (let ((task-id (schedule-task 10 "target" 'test '()
                                   #:daemon-name "test-cancel")))
        (sleep 1)
        (cancel-task task-id #:daemon-name "test-cancel")
        (sleep 1)
        (let ((cancelled (daemon-get-metric timer 'tasks-cancelled)))
          (daemon-stop timer)
          (and cancelled (> cancelled 0)))))))

(test-end "dan9-additional-daemons")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
