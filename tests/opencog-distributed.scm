;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 OpenCog Orchestration Contributors
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

(define-module (test-opencog-distributed)
  #:use-module (gnu opencog distributed)
  #:use-module (srfi srfi-64))

(test-begin "opencog-distributed")

(test-assert "distributed-coordinator creation"
  (let ((coord (make-distributed-coordinator)))
    (coordinator? coord)))

(test-assert "node registration"
  (let ((coord (make-distributed-coordinator)))
    (coordinator-register-node coord 'node-1 "localhost:5000" '(reasoning learning))
    (let ((nodes (coordinator-get-nodes coord)))
      (= (length nodes) 1))))

(test-assert "node unregistration"
  (let ((coord (make-distributed-coordinator)))
    (coordinator-register-node coord 'node-1 "localhost:5000" '())
    (coordinator-unregister-node coord 'node-1)
    (null? (coordinator-get-nodes coord))))

(test-assert "message sending"
  (let ((coord (make-distributed-coordinator)))
    (coordinator-register-node coord 'node-1 "localhost:5000" '())
    (coordinator-send-message coord 'sender 'node-1 'test-msg "payload")))

(test-assert "broadcast"
  (let ((coord (make-distributed-coordinator)))
    (coordinator-register-node coord 'node-1 "localhost:5000" '())
    (coordinator-register-node coord 'node-2 "localhost:5001" '())
    (coordinator-broadcast coord 'sender 'broadcast-msg "data")))

(test-assert "styx-protocol creation"
  (let ((proto (make-styx-protocol)))
    (styx-protocol? proto)))

(test-assert "styx connection"
  (let ((proto (make-styx-protocol)))
    (let ((conn (styx-connect proto "local" "remote")))
      (and (styx-connection? conn)
           (string=? (styx-local conn) "local")
           (string=? (styx-remote conn) "remote")))))

(test-assert "styx disconnect"
  (let ((proto (make-styx-protocol)))
    (let ((conn (styx-connect proto "local" "remote")))
      (styx-disconnect proto conn)
      #t)))

(test-end "opencog-distributed")
