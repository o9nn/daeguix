#!/usr/bin/env -S guile --no-auto-compile
!#
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

;;;
;;; Dan9 Example: Demonstrating daemon-centric architecture
;;;

(add-to-load-path (dirname (current-filename)))

(use-modules (gnu dan9 daemons)
             (gnu dan9 filesystem)
             (gnu dan9 network)
             (gnu dan9 process)
             (gnu dan9 namespace))

(define (main)
  (display "=== Dan9 Daemon Infrastructure Example ===\n\n")
  
  ;; Create core daemons
  (display "Creating core daemons...\n")
  (define fs-daemon (make-filesystem-daemon #:name "filesystem"))
  (define net-daemon (make-network-daemon #:name "network"))
  (define proc-daemon (make-process-daemon #:name "process"))
  (define ns-daemon (make-namespace-daemon #:name "namespace"))
  
  (display "  ✓ Filesystem daemon created\n")
  (display "  ✓ Network daemon created\n")
  (display "  ✓ Process daemon created\n")
  (display "  ✓ Namespace daemon created\n\n")
  
  ;; List all registered daemons
  (display "Registered daemons:\n")
  (for-each (lambda (name)
              (format #t "  - ~a\n" name))
            (daemon-list-all))
  (newline)
  
  ;; Start daemons
  (display "Starting daemons...\n")
  (daemon-start fs-daemon filesystem-daemon-loop)
  (daemon-start net-daemon network-daemon-loop)
  (daemon-start proc-daemon process-daemon-loop)
  (daemon-start ns-daemon namespace-daemon-loop)
  (display "  All daemons started\n\n")
  
  ;; Give daemons time to start
  (sleep 1)
  
  ;; Send some example messages
  (display "Sending example messages...\n")
  
  ;; Create a client daemon to send messages
  (define client (make-daemon "client" 'client #:auto-register #f))
  
  ;; Send filesystem read request
  (daemon-send-message client fs-daemon 'read 
                      '((path . "/etc/config")))
  (display "  ✓ Sent filesystem read request\n")
  
  ;; Send network connect request
  (daemon-send-message client net-daemon 'connect
                      '((host . "example.com") (port . 80)))
  (display "  ✓ Sent network connect request\n")
  
  ;; Send process spawn request
  (daemon-send-message client proc-daemon 'spawn
                      '((command . "ls") (args . ("-la"))))
  (display "  ✓ Sent process spawn request\n")
  
  ;; Send namespace bind request
  (daemon-send-message client ns-daemon 'bind
                      '((path . "/net") (daemon-name . "network")))
  (display "  ✓ Sent namespace bind request\n\n")
  
  ;; Give daemons time to process messages
  (sleep 1)
  
  ;; Display daemon statuses
  (display "Daemon statuses:\n")
  (for-each
   (lambda (daemon)
     (let ((status (daemon-status daemon)))
       (format #t "  ~a: ~a\n"
               (assoc-ref status 'name)
               (assoc-ref status 'state))))
   (list fs-daemon net-daemon proc-daemon ns-daemon))
  (newline)
  
  ;; Display metrics
  (display "Filesystem daemon metrics:\n")
  (for-each
   (lambda (metric)
     (format #t "  ~a: ~a\n" (car metric) (cdr metric)))
   (daemon-get-metrics fs-daemon))
  (newline)
  
  ;; Stop daemons
  (display "Stopping daemons...\n")
  (daemon-stop fs-daemon)
  (daemon-stop net-daemon)
  (daemon-stop proc-daemon)
  (daemon-stop ns-daemon)
  (display "  All daemons stopped\n\n")
  
  (display "=== Dan9 Example Complete ===\n")
  (display "\nDan9 demonstrates 'everything is a daemon':\n")
  (display "  - Uniform daemon interface\n")
  (display "  - Message-based communication\n")
  (display "  - Namespace management\n")
  (display "  - Thread-safe operations\n")
  (display "  - Metrics collection\n\n"))

;; Run the example
(main)
