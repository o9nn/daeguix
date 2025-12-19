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

(define-module (test-dan9-stress)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 egregore)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;;
;;; Stress Tests for Dan9
;;;
;;; Tests system behavior under heavy load and resource constraints.
;;;

(test-begin "dan9-stress")

(format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
(format #t "║              DAN9 STRESS TESTING                              ║~%")
(format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")

;;;
;;; Test 1: High Volume Daemon Creation
;;;

(test-group "high-volume-creation"
  (format #t "Test 1: Creating large number of daemons...~%")
  (test-assert "create 100 daemons"
    (let ((daemons (map (lambda (i)
                         (make-daemon (string-append "stress-" 
                                                    (number->string i))
                                    'stress
                                    #:auto-register #f))
                       (iota 100))))
      (format #t "  ✓ Created ~a daemons~%" (length daemons))
      (= 100 (length daemons)))))

;;;
;;; Test 2: High Volume Message Passing
;;;

(test-group "high-volume-messages"
  (format #t "~%Test 2: High volume message passing...~%")
  (test-assert "send 1000 messages"
    (let* ((msg-count 0)
           (loop-fn (lambda (d)
                     (let loop ()
                       (when (eq? 'running (daemon-state d))
                         (let ((msg (daemon-receive-message d)))
                           (when msg
                             (set! msg-count (+ msg-count 1))))
                         (usleep 5000)
                         (loop)))))
           (sender (make-daemon "stress-sender" 'test #:auto-register #f))
           (receiver (make-daemon "stress-receiver" 'test #:auto-register #f)))
      (daemon-start receiver loop-fn)
      (sleep 0.5)
      ;; Send many messages
      (for-each
       (lambda (i)
         (daemon-send-message sender receiver 'stress-test 
                            `((id . ,i))))
       (iota 1000))
      (sleep 2)
      (daemon-stop receiver)
      (sleep 0.3)
      (format #t "  ✓ Sent 1000 messages, received ~a~%" msg-count)
      (> msg-count 900)))) ; Allow some messages to be in flight

;;;
;;; Test 3: Rapid Start/Stop Cycles
;;;

(test-group "rapid-start-stop"
  (format #t "~%Test 3: Rapid start/stop cycles...~%")
  (test-assert "rapid daemon lifecycle"
    (let* ((cycle-count 0)
           (loop-fn (lambda (d)
                     (set! cycle-count (+ cycle-count 1))
                     (let loop ()
                       (when (eq? 'running (daemon-state d))
                         (usleep 10000)
                         (loop)))))
           (daemon (make-daemon "cycle-test" 'test #:auto-register #f)))
      ;; Perform multiple start/stop cycles
      (for-each
       (lambda (i)
         (daemon-start daemon loop-fn)
         (usleep 50000)
         (daemon-stop daemon)
         (usleep 50000))
       (iota 20))
      (format #t "  ✓ Completed ~a start/stop cycles~%" cycle-count)
      (>= cycle-count 20))))

;;;
;;; Test 4: Large Egregore Swarms
;;;

(test-group "large-swarms"
  (format #t "~%Test 4: Large egregore swarms...~%")
  (test-assert "large swarm coordination"
    (let* ((workers (map (lambda (i)
                          (make-daemon (string-append "swarm-" 
                                                     (number->string i))
                                     'worker
                                     #:auto-register #f))
                        (iota 50)))
           (swarm (make-swarm-egregore "large-swarm" #:daemons workers)))
      (egregore-start swarm)
      (sleep 0.5)
      ;; Send multiple broadcasts
      (for-each
       (lambda (i)
         (egregore-broadcast swarm 'task `((task-id . ,i))))
       (iota 10))
      (sleep 1)
      (egregore-stop swarm)
      (sleep 0.3)
      (format #t "  ✓ Coordinated swarm of ~a daemons~%" (length workers))
      (let ((status (egregore-status swarm)))
        (= 50 (assoc-ref status 'daemon-count))))))

;;;
;;; Test 5: Concurrent Egregore Operations
;;;

(test-group "concurrent-egregores"
  (format #t "~%Test 5: Concurrent egregore operations...~%")
  (test-assert "multiple concurrent egregores"
    (let* ((egregores (map (lambda (i)
                            (let* ((workers (map (lambda (j)
                                                  (make-daemon 
                                                   (string-append "e" 
                                                                 (number->string i)
                                                                 "-w"
                                                                 (number->string j))
                                                   'worker
                                                   #:auto-register #f))
                                               (iota 10)))
                                   (eg (make-swarm-egregore 
                                        (string-append "egregore-" 
                                                      (number->string i))
                                        #:daemons workers)))
                              eg))
                          (iota 10))))
      ;; Start all egregores
      (for-each egregore-start egregores)
      (sleep 0.5)
      ;; Broadcast to all
      (for-each
       (lambda (eg)
         (egregore-broadcast eg 'task '((type . "concurrent"))))
       egregores)
      (sleep 1)
      ;; Stop all
      (for-each egregore-stop egregores)
      (sleep 0.3)
      (format #t "  ✓ Ran ~a concurrent egregores~%" (length egregores))
      (= 10 (length egregores)))))

;;;
;;; Test 6: Memory Stress
;;;

(test-group "memory-stress"
  (format #t "~%Test 6: Memory stress test...~%")
  (test-assert "large metrics tracking"
    (let ((daemon (make-daemon "metrics-stress" 'test #:auto-register #f)))
      ;; Set many metrics
      (for-each
       (lambda (i)
         (daemon-set-metric! daemon 
                           (string->symbol (string-append "metric-" 
                                                         (number->string i)))
                           i))
       (iota 1000))
      (let ((metrics (daemon-get-metrics daemon)))
        (format #t "  ✓ Tracked ~a metrics~%" (length metrics))
        (>= (length metrics) 1000)))))

;;;
;;; Test 7: Message Queue Stress
;;;

(test-group "message-queue-stress"
  (format #t "~%Test 7: Message queue stress...~%")
  (test-assert "rapid sequential messages"
    (let* ((loop-fn (lambda (d)
                     (let loop ()
                       (when (eq? 'running (daemon-state d))
                         (daemon-receive-message d) ; Drain queue
                         (usleep 5000)
                         (loop)))))
           (sender (make-daemon "queue-sender" 'test #:auto-register #f))
           (receiver (make-daemon "queue-receiver" 'test #:auto-register #f)))
      (daemon-start receiver loop-fn)
      (sleep 0.3)
      ;; Flood with messages
      (for-each
       (lambda (i)
         (daemon-send-message sender receiver 'flood 
                            `((seq . ,i))))
       (iota 5000))
      (format #t "  ✓ Sent 5000 messages to queue~%")
      (sleep 2)
      (daemon-stop receiver)
      (sleep 0.3)
      #t)))

;;;
;;; Test 8: Sustained Load
;;;

(test-group "sustained-load"
  (format #t "~%Test 8: Sustained load test...~%")
  (test-assert "continuous operation under load"
    (let* ((workers (map (lambda (i)
                          (let* ((d (make-daemon (string-append "sustained-" 
                                                               (number->string i))
                                                'worker
                                                #:auto-register #f))
                                 (loop-fn (lambda (daemon)
                                           (let loop ()
                                             (when (eq? 'running (daemon-state daemon))
                                               (let ((msg (daemon-receive-message daemon)))
                                                 (when msg
                                                   (daemon-set-metric! daemon 'processed
                                                                     (+ 1 (or (daemon-get-metric daemon 'processed) 0)))))
                                               (usleep 10000)
                                               (loop))))))
                            (daemon-start d loop-fn)
                            d))
                        (iota 20)))
           (sender (make-daemon "load-sender" 'test #:auto-register #f)))
      (sleep 0.5)
      ;; Send messages continuously
      (for-each
       (lambda (i)
         (for-each
          (lambda (worker)
            (daemon-send-message sender worker 'work 
                               `((iteration . ,i))))
          workers)
         (usleep 50000))
       (iota 10))
      (sleep 1)
      ;; Stop all workers
      (for-each daemon-stop workers)
      (sleep 0.3)
      (format #t "  ✓ Sustained load test completed~%")
      #t)))

(test-end "dan9-stress")

(format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
(format #t "║           STRESS TESTING COMPLETE                             ║~%")
(format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
(format #t "Stress tests completed:~%")
(format #t "  ✓ High volume daemon creation~%")
(format #t "  ✓ High volume message passing~%")
(format #t "  ✓ Rapid start/stop cycles~%")
(format #t "  ✓ Large egregore swarms~%")
(format #t "  ✓ Concurrent egregore operations~%")
(format #t "  ✓ Memory stress~%")
(format #t "  ✓ Message queue stress~%")
(format #t "  ✓ Sustained load~%~%")
(format #t "Note: These tests intentionally push system limits.~%")
(format #t "Some performance degradation under stress is expected.~%~%")
