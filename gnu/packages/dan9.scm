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

(define-module (gnu packages dan9)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages plan9))

;;;
;;; Dan9 - Daemon-centric architecture inspired by Plan9
;;;
;;; Where Plan9 says "everything is a file", Dan9 says "everything is a daemon".
;;; Instead of accessing resources through file descriptors, dan9 accesses
;;; resources through daemon message passing.
;;;

(define-public dan9
  (package
    (name "dan9")
    (version "0.1.0")
    (source (local-file "../dan9" #:recursive? #t))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               
               ;; Create output directory structure
               (let* ((out #$output)
                      (lib (string-append out "/lib/guile/3.0/site"))
                      (dan9-dir (string-append lib "/gnu/dan9")))
                 
                 ;; Create directories
                 (mkdir-p dan9-dir)
                 
                 ;; Copy dan9 modules from source
                 (for-each
                  (lambda (module)
                    (let ((source (string-append #$source "/" module))
                          (target (string-append dan9-dir "/" module)))
                      (when (file-exists? source)
                        (copy-file source target))))
                  '("daemons.scm"
                    "filesystem.scm"
                    "network.scm"
                    "process.scm"
                    "namespace.scm"))
                 
                 ;; Create a simple wrapper script
                 (let ((bin (string-append out "/bin")))
                   (mkdir-p bin)
                   (call-with-output-file (string-append bin "/dan9")
                     (lambda (port)
                       (format port "#!~a/bin/guile --no-auto-compile~%!#~%"
                               #$guile-3.0)
                       (format port "(add-to-load-path \"~a\")~%" lib)
                       (format port "(use-modules (gnu dan9 daemons))~%")
                       (format port "(use-modules (gnu dan9 filesystem))~%")
                       (format port "(use-modules (gnu dan9 network))~%")
                       (format port "(use-modules (gnu dan9 process))~%")
                       (format port "(use-modules (gnu dan9 namespace))~%")
                       (format port "(display \"Dan9 daemon infrastructure loaded.\\n\")~%")))
                   (chmod (string-append bin "/dan9") #o755))
                 
                 #t))))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-3.0))
    (home-page "https://github.com/cogpy/daeguix")
    (synopsis "Daemon-centric architecture inspired by Plan9")
    (description
     "Dan9 is a daemon-centric architecture inspired by Plan9's file-centric
architecture.  Where Plan9 says \"everything is a file\", Dan9 says
\"everything is a daemon\".

Instead of accessing resources through file descriptors, dan9 accesses
resources through daemon message passing.  This package provides:

@itemize
@item Core daemon infrastructure for creating and managing daemons
@item Filesystem daemon for file operations through message passing
@item Network daemon for network operations through message passing
@item Process daemon for process management through message passing
@item Namespace daemon for daemon binding and namespace management
@end itemize

Dan9 daemons communicate through message passing, similar to how Plan9
processes communicate through file operations.  This creates a uniform
interface where all system resources are accessed as daemon services.")
    (license license:gpl3+)))
