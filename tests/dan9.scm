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

(define-module (test-dan9)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 filesystem)
  #:use-module (gnu dan9 network)
  #:use-module (gnu dan9 process)
  #:use-module (gnu dan9 namespace)
  #:use-module (srfi srfi-64))

;;;
;;; Tests for Dan9 daemon infrastructure
;;;

(test-begin "dan9")

;;;
;;; Core daemon tests
;;;

(test-assert "create daemon"
  (let ((daemon (make-daemon "test-daemon" 'test)))
    (and (daemon? daemon)
         (equal? (daemon-name daemon) "test-daemon")
         (equal? (daemon-type daemon) 'test)
         (equal? (daemon-state daemon) 'stopped))))

(test-assert "daemon registry"
  (let ((daemon (make-daemon "registry-test" 'test)))
    (daemon-register daemon)
    (let ((found (daemon-lookup "registry-test")))
      (and found
           (equal? (daemon-name found) "registry-test")))))

(test-assert "daemon list all"
  (let ((daemons-before (daemon-list-all)))
    (make-daemon "list-test" 'test)
    (let ((daemons-after (daemon-list-all)))
      (> (length daemons-after) (length daemons-before)))))

(test-assert "daemon metrics"
  (let ((daemon (make-daemon "metrics-test" 'test #:auto-register #f)))
    (daemon-set-metric! daemon 'test-metric 42)
    (equal? (daemon-get-metric daemon 'test-metric) 42)))

(test-assert "daemon status"
  (let* ((daemon (make-daemon "status-test" 'test #:auto-register #f))
         (status (daemon-status daemon)))
    (and (assoc-ref status 'name)
         (assoc-ref status 'type)
         (assoc-ref status 'state))))

;;;
;;; Filesystem daemon tests
;;;

(test-assert "create filesystem daemon"
  (let ((daemon (make-filesystem-daemon #:name "fs-test")))
    (and (daemon? daemon)
         (equal? (daemon-type daemon) 'filesystem))))

;;;
;;; Network daemon tests
;;;

(test-assert "create network daemon"
  (let ((daemon (make-network-daemon #:name "net-test")))
    (and (daemon? daemon)
         (equal? (daemon-type daemon) 'network))))

;;;
;;; Process daemon tests
;;;

(test-assert "create process daemon"
  (let ((daemon (make-process-daemon #:name "proc-test")))
    (and (daemon? daemon)
         (equal? (daemon-type daemon) 'process))))

;;;
;;; Namespace daemon tests
;;;

(test-assert "create namespace daemon"
  (let ((daemon (make-namespace-daemon #:name "ns-test")))
    (and (daemon? daemon)
         (equal? (daemon-type daemon) 'namespace))))

(test-end "dan9")
