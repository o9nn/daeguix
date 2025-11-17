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

(define-module (test-egregore)
  #:use-module (gnu dan9 daemons)
  #:use-module (gnu dan9 egregore)
  #:use-module (srfi srfi-64))

;;;
;;; Tests for Dan9 Egregore daemon orchestration
;;;

(test-begin "egregore")

;;;
;;; Egregore creation tests
;;;

(test-assert "create swarm egregore"
  (let ((egregore (make-swarm-egregore "test-swarm")))
    (and (egregore? egregore)
         (equal? (egregore-name egregore) "test-swarm")
         (equal? (egregore-archetype egregore) 'swarm))))

(test-assert "create hierarchy egregore"
  (let ((egregore (make-hierarchy-egregore "test-hierarchy")))
    (and (egregore? egregore)
         (equal? (egregore-archetype egregore) 'hierarchy))))

(test-assert "create ring egregore"
  (let ((egregore (make-ring-egregore "test-ring")))
    (and (egregore? egregore)
         (equal? (egregore-archetype egregore) 'ring))))

(test-assert "create mesh egregore"
  (let ((egregore (make-mesh-egregore "test-mesh")))
    (and (egregore? egregore)
         (equal? (egregore-archetype egregore) 'mesh))))

;;;
;;; Daemon management tests
;;;

(test-assert "add daemon to egregore"
  (let ((egregore (make-swarm-egregore "test-add"))
        (daemon (make-daemon "test-daemon" 'test #:auto-register #f)))
    (egregore-add-daemon! egregore daemon)
    (member daemon (egregore-daemons egregore))))

(test-assert "remove daemon from egregore"
  (let ((egregore (make-swarm-egregore "test-remove"))
        (daemon (make-daemon "test-daemon" 'test #:auto-register #f)))
    (egregore-add-daemon! egregore daemon)
    (egregore-remove-daemon! egregore daemon)
    (not (member daemon (egregore-daemons egregore)))))

(test-assert "egregore with initial daemons"
  (let* ((d1 (make-daemon "d1" 'test #:auto-register #f))
         (d2 (make-daemon "d2" 'test #:auto-register #f))
         (egregore (make-swarm-egregore "test-initial"
                                       #:daemons (list d1 d2))))
    (= 2 (length (egregore-daemons egregore)))))

;;;
;;; Egregore status tests
;;;

(test-assert "egregore status"
  (let* ((daemon (make-daemon "test-daemon" 'test #:auto-register #f))
         (egregore (make-swarm-egregore "test-status"
                                       #:daemons (list daemon)))
         (status (egregore-status egregore)))
    (and (assoc-ref status 'name)
         (assoc-ref status 'archetype)
         (assoc-ref status 'daemon-count)
         (= 1 (assoc-ref status 'daemon-count)))))

;;;
;;; Coordinator tests
;;;

(test-assert "egregore has coordinator"
  (let ((egregore (make-swarm-egregore "test-coord")))
    (and (egregore-coordinator egregore)
         (daemon? (egregore-coordinator egregore))
         (equal? (daemon-type (egregore-coordinator egregore))
                'egregore-coordinator))))

(test-assert "start and stop egregore"
  (let ((egregore (make-swarm-egregore "test-lifecycle")))
    (egregore-start egregore)
    (sleep 0.1)
    (let ((running (eq? 'running 
                        (daemon-state (egregore-coordinator egregore)))))
      (egregore-stop egregore)
      (sleep 0.1)
      running)))

;;;
;;; Archetype-specific tests
;;;

(test-assert "all archetypes can be created"
  (let ((swarm (make-swarm-egregore "s"))
        (hierarchy (make-hierarchy-egregore "h"))
        (ring (make-ring-egregore "r"))
        (mesh (make-mesh-egregore "m")))
    (and (egregore? swarm)
         (egregore? hierarchy)
         (egregore? ring)
         (egregore? mesh))))

(test-end "egregore")
