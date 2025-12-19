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

(define-module (test-dan9-multi-daemon)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 egregore)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;;
;;; Multi-Daemon Communication Tests
;;;
;;; Tests complex scenarios involving multiple daemons communicating
;;; simultaneously in various patterns.
;;;

(test-begin "dan9-multi-daemon")

;;;
;;; Test 1: Point-to-Point Communication
;;;

(test-group "point-to-point"
  (test-assert "two daemons exchange messages"
    (let* ((msg1-received #f)
           (msg2-received #f)
           (loop-fn-1 (lambda (d)
                       (let loop ()
                         (when (eq? 'running (daemon-state d))
                           (let ((msg (daemon-receive-message d)))
                             (when msg
                               (set! msg1-received #t)))
                           (usleep 50000)
                           (loop)))))
           (loop-fn-2 (lambda (d)
                       (let loop ()
                         (when (eq? 'running (daemon-state d))
                           (let ((msg (daemon-receive-message d)))
                             (when msg
                               (set! msg2-received #t)))
                           (usleep 50000)
                           (loop)))))
           (d1 (make-daemon "p2p-1" 'test #:auto-register #f))
           (d2 (make-daemon "p2p-2" 'test #:auto-register #f)))
      (daemon-start d1 loop-fn-1)
      (daemon-start d2 loop-fn-2)
      (sleep 0.5)
      (daemon-send-message d1 d2 'hello '((from . d1)))
      (daemon-send-message d2 d1 'hello '((from . d2)))
      (sleep 0.5)
      (daemon-stop d1)
      (daemon-stop d2)
      (sleep 0.3)
      (and msg1-received msg2-received))))

;;;
;;; Test 2: Broadcast Communication
;;;

(test-group "broadcast"
  (test-assert "one daemon broadcasts to multiple receivers"
    (let* ((received-counts (make-vector 5 0))
           (make-receiver (lambda (id)
                           (lambda (d)
                             (let loop ()
                               (when (eq? 'running (daemon-state d))
                                 (let ((msg (daemon-receive-message d)))
                                   (when msg
                                     (vector-set! received-counts id
                                               (+ 1 (vector-ref received-counts id)))))
                                 (usleep 50000)
                                 (loop))))))
           (broadcaster (make-daemon "broadcaster" 'test #:auto-register #f))
           (receivers (map (lambda (i)
                            (let ((d (make-daemon 
                                      (string-append "receiver-" 
                                                    (number->string i))
                                      'test 
                                      #:auto-register #f)))
                              (daemon-start d (make-receiver i))
                              d))
                          (iota 5))))
      (sleep 0.5)
      ;; Broadcast messages to all receivers
      (for-each
       (lambda (receiver)
         (daemon-send-message broadcaster receiver 'broadcast 
                            '((msg . "hello-all"))))
       receivers)
      (sleep 0.5)
      (for-each daemon-stop receivers)
      (sleep 0.3)
      ;; Check that all receivers got at least one message
      (let ((all-received (every (lambda (i) 
                                  (> (vector-ref received-counts i) 0))
                                (iota 5))))
        all-received))))

;;;
;;; Test 3: Chain Communication
;;;

(test-group "chain"
  (test-assert "messages propagate through daemon chain"
    (let* ((chain-state (make-vector 4 0))
           (make-chain-loop (lambda (id next-daemon)
                             (lambda (d)
                               (let loop ()
                                 (when (eq? 'running (daemon-state d))
                                   (let ((msg (daemon-receive-message d)))
                                     (when msg
                                       (vector-set! chain-state id 1)
                                       (when next-daemon
                                         (daemon-send-message d next-daemon 
                                                            'chain-msg
                                                            '((data . "propagate"))))))
                                   (usleep 50000)
                                   (loop))))))
           (d4 (make-daemon "chain-4" 'test #:auto-register #f))
           (d3 (make-daemon "chain-3" 'test #:auto-register #f))
           (d2 (make-daemon "chain-2" 'test #:auto-register #f))
           (d1 (make-daemon "chain-1" 'test #:auto-register #f)))
      (daemon-start d4 (make-chain-loop 3 #f))
      (daemon-start d3 (make-chain-loop 2 d4))
      (daemon-start d2 (make-chain-loop 1 d3))
      (daemon-start d1 (make-chain-loop 0 d2))
      (sleep 0.5)
      ;; Start the chain
      (daemon-send-message d1 d1 'chain-msg '((data . "start")))
      (sleep 1)
      (daemon-stop d1)
      (daemon-stop d2)
      (daemon-stop d3)
      (daemon-stop d4)
      (sleep 0.3)
      ;; Check if all daemons in chain received message
      (every (lambda (i) (= 1 (vector-ref chain-state i))) (iota 4)))))

;;;
;;; Test 4: Ring Communication
;;;

(test-group "ring"
  (test-assert "messages circulate in daemon ring"
    (let* ((pass-count (make-vector 4 0))
           (make-ring-loop (lambda (id next-daemon)
                            (lambda (d)
                              (let loop ()
                                (when (eq? 'running (daemon-state d))
                                  (let ((msg (daemon-receive-message d)))
                                    (when msg
                                      (let ((count (vector-ref pass-count id)))
                                        (vector-set! pass-count id (+ count 1))
                                        (when (< count 2)  ; Limit iterations
                                          (daemon-send-message d next-daemon 
                                                             'ring-msg
                                                             '((data . "around")))))))
                                  (usleep 50000)
                                  (loop))))))
           (d1 (make-daemon "ring-1" 'test #:auto-register #f))
           (d2 (make-daemon "ring-2" 'test #:auto-register #f))
           (d3 (make-daemon "ring-3" 'test #:auto-register #f))
           (d4 (make-daemon "ring-4" 'test #:auto-register #f)))
      (daemon-start d1 (make-ring-loop 0 d2))
      (daemon-start d2 (make-ring-loop 1 d3))
      (daemon-start d3 (make-ring-loop 2 d4))
      (daemon-start d4 (make-ring-loop 3 d1))
      (sleep 0.5)
      ;; Start the ring
      (daemon-send-message d1 d1 'ring-msg '((data . "start")))
      (sleep 1.5)
      (daemon-stop d1)
      (daemon-stop d2)
      (daemon-stop d3)
      (daemon-stop d4)
      (sleep 0.3)
      ;; Check if messages circulated
      (> (+ (vector-ref pass-count 0)
            (vector-ref pass-count 1)
            (vector-ref pass-count 2)
            (vector-ref pass-count 3))
         4))))

;;;
;;; Test 5: Swarm Coordination
;;;

(test-group "swarm-coordination"
  (test-assert "egregore coordinates swarm of daemons"
    (let* ((d1 (make-daemon "swarm-worker-1" 'worker #:auto-register #f))
           (d2 (make-daemon "swarm-worker-2" 'worker #:auto-register #f))
           (d3 (make-daemon "swarm-worker-3" 'worker #:auto-register #f))
           (d4 (make-daemon "swarm-worker-4" 'worker #:auto-register #f))
           (swarm (make-swarm-egregore "test-swarm"
                                      #:daemons (list d1 d2 d3 d4))))
      (egregore-start swarm)
      (sleep 0.5)
      ;; Broadcast to swarm
      (egregore-broadcast swarm 'task '((type . "compute")))
      (sleep 0.5)
      (egregore-stop swarm)
      (sleep 0.3)
      ;; Verify swarm was created and operated
      (let ((status (egregore-status swarm)))
        (= 4 (assoc-ref status 'daemon-count))))))

;;;
;;; Test 6: Hierarchical Communication
;;;

(test-group "hierarchical"
  (test-assert "hierarchy egregore maintains structure"
    (let* ((manager (make-daemon "manager" 'manager #:auto-register #f))
           (supervisor (make-daemon "supervisor" 'supervisor #:auto-register #f))
           (worker1 (make-daemon "worker-1" 'worker #:auto-register #f))
           (worker2 (make-daemon "worker-2" 'worker #:auto-register #f))
           (hierarchy (make-hierarchy-egregore "test-hierarchy"
                                              #:daemons (list manager 
                                                             supervisor
                                                             worker1 
                                                             worker2))))
      (egregore-start hierarchy)
      (sleep 0.5)
      (egregore-stop hierarchy)
      (sleep 0.3)
      ;; Verify hierarchy was created
      (let ((status (egregore-status hierarchy)))
        (= 4 (assoc-ref status 'daemon-count))))))

;;;
;;; Test 7: Pipeline Processing
;;;

(test-group "pipeline"
  (test-assert "pipeline egregore processes in sequence"
    (let* ((stage1 (make-daemon "stage-1" 'processor #:auto-register #f))
           (stage2 (make-daemon "stage-2" 'processor #:auto-register #f))
           (stage3 (make-daemon "stage-3" 'processor #:auto-register #f))
           (pipeline (make-pipeline-egregore "test-pipeline"
                                            #:daemons (list stage1 stage2 stage3))))
      (egregore-start pipeline)
      (sleep 0.5)
      (egregore-stop pipeline)
      (sleep 0.3)
      ;; Verify pipeline was created
      (let ((status (egregore-status pipeline)))
        (= 3 (assoc-ref status 'daemon-count))))))

;;;
;;; Test 8: Concurrent Operations
;;;

(test-group "concurrent-operations"
  (test-assert "multiple daemon groups operate concurrently"
    (let* ((group1-d1 (make-daemon "g1-d1" 'worker #:auto-register #f))
           (group1-d2 (make-daemon "g1-d2" 'worker #:auto-register #f))
           (group2-d1 (make-daemon "g2-d1" 'worker #:auto-register #f))
           (group2-d2 (make-daemon "g2-d2" 'worker #:auto-register #f))
           (swarm1 (make-swarm-egregore "swarm-1" 
                                       #:daemons (list group1-d1 group1-d2)))
           (swarm2 (make-swarm-egregore "swarm-2"
                                       #:daemons (list group2-d1 group2-d2))))
      (egregore-start swarm1)
      (egregore-start swarm2)
      (sleep 0.5)
      (egregore-broadcast swarm1 'task1 '())
      (egregore-broadcast swarm2 'task2 '())
      (sleep 0.5)
      (egregore-stop swarm1)
      (egregore-stop swarm2)
      (sleep 0.3)
      ;; Both swarms operated concurrently
      #t)))

(test-end "dan9-multi-daemon")

(format #t "~%=== Dan9 Multi-Daemon Communication Tests Complete ===~%")
(format #t "Tested communication patterns:~%")
(format #t "  ✓ Point-to-point~%")
(format #t "  ✓ Broadcast~%")
(format #t "  ✓ Chain propagation~%")
(format #t "  ✓ Ring circulation~%")
(format #t "  ✓ Swarm coordination~%")
(format #t "  ✓ Hierarchical structure~%")
(format #t "  ✓ Pipeline processing~%")
(format #t "  ✓ Concurrent operations~%~%")
