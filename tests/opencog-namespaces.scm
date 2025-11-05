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

(define-module (test-opencog-namespaces)
  #:use-module (gnu opencog namespaces)
  #:use-module (srfi srfi-64))

(test-begin "opencog-namespaces")

(test-assert "namespace-manager creation"
  (let ((ns (make-namespace-manager #:root "/")))
    (namespace? ns)))

(test-assert "namespace bind and lookup"
  (let ((ns (make-namespace-manager)))
    (namespace-bind ns "/test/path" 'test-resource 'file '((size . 100)))
    (let ((entry (namespace-lookup ns "/test/path")))
      (and entry
           (namespace-entry? entry)
           (string=? (entry-path entry) "/test/path")
           (eq? (entry-type entry) 'file)
           (eq? (entry-resource entry) 'test-resource)))))

(test-assert "namespace unbind"
  (let ((ns (make-namespace-manager)))
    (namespace-bind ns "/test/remove" 'resource 'file '())
    (namespace-unbind ns "/test/remove")
    (not (namespace-lookup ns "/test/remove"))))

(test-assert "namespace mount"
  (let ((ns (make-namespace-manager)))
    (namespace-bind ns "/source" 'source-resource 'file '())
    (namespace-mount ns "/source" "/target")
    (let ((target-entry (namespace-lookup ns "/target")))
      (and target-entry
           (eq? (entry-resource target-entry) 'source-resource)))))

(test-assert "namespace walk"
  (let ((ns (make-namespace-manager)))
    (namespace-bind ns "/test/a" 'res-a 'file '())
    (namespace-bind ns "/test/b" 'res-b 'file '())
    (namespace-bind ns "/other/c" 'res-c 'file '())
    (let ((entries (namespace-walk ns "/test/")))
      (= (length entries) 2))))

(test-assert "9p-service creation"
  (let* ((ns (make-namespace-manager))
         ;; Use default 9P port
         (svc (make-9p-service ns)))
    (9p-service? svc)))

(test-assert "9p-service start and stop"
  (let* ((ns (make-namespace-manager))
         (svc (make-9p-service ns)))
    (9p-service-start svc)
    (sleep 1)
    (9p-service-stop svc)
    #t))

(test-end "opencog-namespaces")
