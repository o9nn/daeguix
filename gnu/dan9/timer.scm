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

(define-module (gnu dan9 timer)
  #:use-module (gnu dan9 daemons)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-timer-daemon
            timer-daemon-loop
            schedule-task
            schedule-repeating-task
            cancel-task
            list-scheduled-tasks
            make-scheduled-task
            scheduled-task?
            scheduled-task-id
            scheduled-task-interval
            scheduled-task-repeating
            scheduled-task-target
            scheduled-task-message-type
            scheduled-task-payload))

;;;
;;; Dan9 Timer Module
;;;
;;; Provides scheduled task execution for daemons.
;;; Allows daemons to schedule messages to be sent at future times.
;;;

;;; Task ID counter

(define *task-id-counter* 0)
(define *task-id-mutex* (make-mutex))

(define (next-task-id)
  "Generate next task ID."
  (with-mutex *task-id-mutex*
    (set! *task-id-counter* (+ *task-id-counter* 1))
    *task-id-counter*))

;;; Scheduled task record

(define-record-type <scheduled-task>
  (make-scheduled-task-internal id interval repeating target message-type
                               payload next-fire last-fire)
  scheduled-task?
  (id scheduled-task-id)
  (interval scheduled-task-interval)
  (repeating scheduled-task-repeating)
  (target scheduled-task-target)
  (message-type scheduled-task-message-type)
  (payload scheduled-task-payload)
  (next-fire scheduled-task-next-fire set-scheduled-task-next-fire!)
  (last-fire scheduled-task-last-fire set-scheduled-task-last-fire!))

(define* (make-scheduled-task interval target message-type payload
                             #:key (repeating #f))
  "Create a scheduled task."
  (let ((now (time-second (current-time time-monotonic))))
    (make-scheduled-task-internal
     (next-task-id)
     interval
     repeating
     target
     message-type
     payload
     (+ now interval)
     #f)))

;;; Task storage

(define *scheduled-tasks* '())
(define *scheduled-tasks-mutex* (make-mutex))

(define (add-scheduled-task! task)
  "Add a task to the schedule."
  (with-mutex *scheduled-tasks-mutex*
    (set! *scheduled-tasks* (cons task *scheduled-tasks*))
    (scheduled-task-id task)))

(define (remove-scheduled-task! task-id)
  "Remove a task from the schedule."
  (with-mutex *scheduled-tasks-mutex*
    (set! *scheduled-tasks*
          (filter (lambda (t)
                   (not (= (scheduled-task-id t) task-id)))
                 *scheduled-tasks*))
    #t))

(define (get-scheduled-tasks)
  "Get all scheduled tasks."
  (with-mutex *scheduled-tasks-mutex*
    *scheduled-tasks*))

(define (get-due-tasks)
  "Get tasks that are due to fire."
  (let ((now (time-second (current-time time-monotonic))))
    (with-mutex *scheduled-tasks-mutex*
      (filter (lambda (task)
               (<= (scheduled-task-next-fire task) now))
             *scheduled-tasks*))))

;;;
;;; Task Scheduling API
;;;

(define* (schedule-task interval target message-type payload
                       #:key (daemon-name "timer"))
  "Schedule a one-time task."
  (let* ((timer (daemon-lookup daemon-name))
         (task (make-scheduled-task interval target message-type payload
                                   #:repeating #f)))
    (when timer
      (daemon-send-message timer timer 'schedule
                          `((task . ,task)))
      (scheduled-task-id task))))

(define* (schedule-repeating-task interval target message-type payload
                                 #:key (daemon-name "timer"))
  "Schedule a repeating task."
  (let* ((timer (daemon-lookup daemon-name))
         (task (make-scheduled-task interval target message-type payload
                                   #:repeating #t)))
    (when timer
      (daemon-send-message timer timer 'schedule
                          `((task . ,task)))
      (scheduled-task-id task))))

(define* (cancel-task task-id #:key (daemon-name "timer"))
  "Cancel a scheduled task."
  (let ((timer (daemon-lookup daemon-name)))
    (when timer
      (daemon-send-message timer timer 'cancel
                          `((task-id . ,task-id))))))

(define* (list-scheduled-tasks #:key (daemon-name "timer"))
  "List all scheduled tasks."
  (let ((timer (daemon-lookup daemon-name)))
    (when timer
      (daemon-send-message timer timer 'list '()))))

;;;
;;; Task Execution
;;;

(define (fire-task! task sender)
  "Fire a scheduled task by sending its message."
  (let ((target (daemon-lookup (scheduled-task-target task))))
    (when target
      (daemon-send-message sender target
                          (scheduled-task-message-type task)
                          (scheduled-task-payload task))
      (set-scheduled-task-last-fire! task
                                    (time-second (current-time time-monotonic)))
      
      ;; Reschedule if repeating
      (when (scheduled-task-repeating task)
        (set-scheduled-task-next-fire! task
                                      (+ (scheduled-task-last-fire task)
                                         (scheduled-task-interval task)))))))

(define (process-due-tasks daemon)
  "Process all tasks that are due."
  (let ((due-tasks (get-due-tasks)))
    (for-each
     (lambda (task)
       (fire-task! task daemon)
       (daemon-set-metric! daemon 'tasks-fired
                         (+ 1 (or (daemon-get-metric daemon 'tasks-fired) 0)))
       
       ;; Remove if not repeating
       (unless (scheduled-task-repeating task)
         (remove-scheduled-task! (scheduled-task-id task))))
     due-tasks)))

;;;
;;; Timer Daemon
;;;

(define* (make-timer-daemon #:key
                           (name "timer")
                           (tick-interval 1))
  "Create a timer daemon for scheduled tasks."
  (make-daemon name 'timer
               #:config `((tick-interval . ,tick-interval))
               #:auto-register #t))

(define (timer-daemon-loop daemon)
  "Main loop for timer daemon."
  (let ((tick-interval (or (assoc-ref (daemon-config daemon) 'tick-interval) 1)))
    (format #t "[~a] Timer daemon started (tick: ~as)~%"
            (daemon-name daemon) tick-interval)
    
    (let loop ((last-tick (current-time time-monotonic)))
      (when (eq? 'running (daemon-state daemon))
        ;; Process messages
        (let ((msg (daemon-receive-message daemon)))
          (when msg
            (handle-timer-message daemon msg)))
        
        ;; Process due tasks
        (let* ((now (current-time time-monotonic))
               (elapsed (- (time-second now) (time-second last-tick))))
          (if (>= elapsed tick-interval)
              (begin
                (process-due-tasks daemon)
                (usleep 100000) ; 100ms
                (loop now))
              (begin
                (usleep 100000) ; 100ms
                (loop last-tick))))))))

(define (handle-timer-message daemon msg)
  "Handle incoming messages for timer daemon."
  (match (message-type msg)
    ('schedule
     (let ((task (assoc-ref (message-payload msg) 'task)))
       (when task
         (let ((task-id (add-scheduled-task! task)))
           (format #t "[~a] Scheduled task #~a (~a, interval: ~as, repeating: ~a)~%"
                   (daemon-name daemon)
                   task-id
                   (scheduled-task-target task)
                   (scheduled-task-interval task)
                   (scheduled-task-repeating task))
           (daemon-set-metric! daemon 'tasks-scheduled
                             (+ 1 (or (daemon-get-metric daemon 'tasks-scheduled) 0)))))))
    
    ('cancel
     (let ((task-id (assoc-ref (message-payload msg) 'task-id)))
       (when task-id
         (remove-scheduled-task! task-id)
         (format #t "[~a] Cancelled task #~a~%"
                 (daemon-name daemon) task-id)
         (daemon-set-metric! daemon 'tasks-cancelled
                           (+ 1 (or (daemon-get-metric daemon 'tasks-cancelled) 0))))))
    
    ('list
     (let ((tasks (get-scheduled-tasks)))
       (format #t "[~a] Scheduled tasks (~a):~%" 
               (daemon-name daemon) (length tasks))
       (for-each
        (lambda (task)
          (let* ((now (time-second (current-time time-monotonic)))
                 (time-until (- (scheduled-task-next-fire task) now)))
            (format #t "  #~a: ~a -> ~a in ~as (~a)~%"
                    (scheduled-task-id task)
                    (scheduled-task-target task)
                    (scheduled-task-message-type task)
                    time-until
                    (if (scheduled-task-repeating task)
                        "repeating"
                        "one-time"))))
        tasks)))
    
    ('stats
     (let ((tasks (get-scheduled-tasks)))
       (format #t "[~a] Stats:~%" (daemon-name daemon))
       (format #t "  Active tasks: ~a~%" (length tasks))
       (format #t "  Tasks scheduled: ~a~%"
               (or (daemon-get-metric daemon 'tasks-scheduled) 0))
       (format #t "  Tasks fired: ~a~%"
               (or (daemon-get-metric daemon 'tasks-fired) 0))
       (format #t "  Tasks cancelled: ~a~%"
               (or (daemon-get-metric daemon 'tasks-cancelled) 0))))
    
    (_ (format #t "[~a] Unknown message type: ~a~%"
               (daemon-name daemon) (message-type msg)))))
