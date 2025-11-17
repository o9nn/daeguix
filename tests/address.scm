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

(define-module (test-address)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 address)
  #:use-module (srfi srfi-64))

;;;
;;; Tests for D9 address system
;;;

(test-begin "address")

;;;
;;; Address creation and parsing tests
;;;

(test-assert "create address"
  (let ((addr (make-address "d9://test/group/daemon-1")))
    (and (address? addr)
         (equal? (address-uri addr) "d9://test/group/daemon-1"))))

(test-assert "parse address"
  (let ((parsed (parse-address "d9://egregore/swarm/worker-1")))
    (and (equal? (assoc-ref parsed 'scheme) "d9")
         (equal? (assoc-ref parsed 'type) "egregore")
         (equal? (assoc-ref parsed 'group) "swarm")
         (equal? (assoc-ref parsed 'name) "worker-1"))))

;;;
;;; Address router tests
;;;

(test-assert "create address router"
  (let ((router (make-address-router)))
    (address-router? router)))

(test-assert "router register and resolve"
  (let ((router (make-address-router))
        (daemon (make-daemon "test-daemon" 'test #:auto-register #f)))
    (router-register! router "d9://test/group/daemon" daemon)
    (let ((addr (router-resolve router "d9://test/group/daemon")))
      (and addr
           (address? addr)
           (eq? (address-daemon addr) daemon)))))

(test-assert "router unregister"
  (let ((router (make-address-router))
        (daemon (make-daemon "test-daemon" 'test #:auto-register #f)))
    (router-register! router "d9://test/group/daemon" daemon)
    (router-unregister! router "d9://test/group/daemon")
    (not (router-resolve router "d9://test/group/daemon"))))

;;;
;;; Thread pool tests
;;;

(test-assert "create thread pool"
  (let ((pool (make-thread-pool "test-pool" #:size 5)))
    (and (thread-pool? pool)
         (equal? (thread-pool-name pool) "test-pool")
         (= (thread-pool-size pool) 5))))

(test-assert "thread pool spawn cycle"
  (let ((pool (make-thread-pool "test-pool")))
    (thread-pool-spawn-cycle! pool (lambda () (usleep 100000)))
    (= (length (thread-pool-cycles pool)) 1)))

(test-assert "thread pool drop cycle"
  (let ((pool (make-thread-pool "test-pool")))
    (let ((thread (thread-pool-spawn-cycle! pool (lambda () (usleep 100000)))))
      (thread-pool-drop-cycle! pool thread)
      (= (length (thread-pool-cycles pool)) 0))))

(test-assert "thread pool sync cycles - scale up"
  (let ((pool (make-thread-pool "test-pool")))
    (thread-pool-sync-cycles! pool 10)
    (= (length (thread-pool-cycles pool)) 10)))

(test-assert "thread pool sync cycles - scale down"
  (let ((pool (make-thread-pool "test-pool")))
    (thread-pool-sync-cycles! pool 10)
    (thread-pool-sync-cycles! pool 3)
    (= (length (thread-pool-cycles pool)) 3)))

(test-end "address")
