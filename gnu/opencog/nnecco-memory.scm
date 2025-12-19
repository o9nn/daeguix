;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 NNECCO-A9NN Contributors
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

(define-module (gnu opencog nnecco-memory)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (gnu opencog daemons)
  #:export (;; AtomSpace Integration
            make-atomspace-daemon
            atomspace-add-node
            atomspace-add-link
            atomspace-query
            atomspace-spread-attention
            
            ;; Episodic Memory
            make-episodic-memory-daemon
            episodic-store
            episodic-recall
            episodic-query
            
            ;; Replay Memory
            make-replay-memory-daemon
            replay-store
            replay-sample
            replay-prioritize
            
            ;; Personality Tensor
            make-personality-daemon
            personality-set-trait
            personality-get-traits
            personality-select-frame))

;;;
;;; AtomSpace Integration Daemon
;;;
;;; Provides hypergraph-based knowledge representation
;;; with attention spreading and relevance realization
;;;

(define-record-type <atom>
  (make-atom-internal type name tv attention metadata)
  atom?
  (type atom-type)
  (name atom-name)
  (tv atom-tv set-atom-tv!)
  (attention atom-attention set-atom-attention!)
  (metadata atom-metadata set-atom-metadata!))

(define-record-type <truth-value>
  (make-truth-value strength confidence)
  truth-value?
  (strength tv-strength)
  (confidence tv-confidence))

(define* (make-atomspace-daemon #:key (name "atomspace"))
  "Create AtomSpace daemon for hypergraph knowledge representation."
  (let ((daemon (make-daemon name 'atomspace #:auto-register #t)))
    
    ;; Initialize AtomSpace storage
    (daemon-set-metric! daemon 'atoms (make-hash-table))
    (daemon-set-metric! daemon 'links (make-hash-table))
    (daemon-set-metric! daemon 'attention-bank '())
    (daemon-set-metric! daemon 'total-atoms 0)
    
    daemon))

(define (atomspace-daemon-loop daemon)
  "Main loop for AtomSpace daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      ;; Process messages
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('add-node
             (let* ((payload (message-payload msg))
                    (type (assoc-ref payload 'type))
                    (name (assoc-ref payload 'name))
                    (tv (assoc-ref payload 'tv))
                    (attention (assoc-ref payload 'attention))
                    (metadata (assoc-ref payload 'metadata))
                    (atom (atomspace-add-node daemon type name tv attention metadata)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'atom-added
                                        `((atom . ,atom))))))))
            
            ('add-link
             (let* ((payload (message-payload msg))
                    (type (assoc-ref payload 'type))
                    (outgoing (assoc-ref payload 'outgoing))
                    (tv (assoc-ref payload 'tv)))
               (atomspace-add-link daemon type outgoing tv)))
            
            ('query
             (let* ((payload (message-payload msg))
                    (pattern (assoc-ref payload 'pattern))
                    (results (atomspace-query daemon pattern)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'query-results
                                        `((results . ,results))))))))
            
            ('spread-attention
             (let* ((payload (message-payload msg))
                    (source-atom (assoc-ref payload 'source)))
               (atomspace-spread-attention daemon source-atom)))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (atomspace-add-node daemon type name tv attention metadata)
  "Add a node to the AtomSpace."
  (let* ((atoms (daemon-get-metric daemon 'atoms))
         (atom-id (string-append (symbol->string type) ":" name))
         (atom (make-atom-internal
                type
                name
                (or tv (make-truth-value 0.5 0.5))
                (or attention 0.0)
                (or metadata '()))))
    
    (hash-set! atoms atom-id atom)
    (daemon-set-metric! daemon 'total-atoms
                       (+ 1 (daemon-get-metric daemon 'total-atoms)))
    
    (format #t "[~a] Added node: ~a (~a)~%"
            (daemon-name daemon) name type)
    
    atom))

(define (atomspace-add-link daemon type outgoing tv)
  "Add a link between atoms in the AtomSpace."
  (let* ((links (daemon-get-metric daemon 'links))
         (link-id (string-append (symbol->string type) ":"
                                (string-join (map atom-name outgoing) ",")))
         (link `((type . ,type)
                (outgoing . ,outgoing)
                (tv . ,(or tv (make-truth-value 0.5 0.5))))))
    
    (hash-set! links link-id link)
    
    (format #t "[~a] Added link: ~a~%"
            (daemon-name daemon) link-id)
    
    link))

(define (atomspace-query daemon pattern)
  "Query atoms matching a pattern."
  (let ((atoms (daemon-get-metric daemon 'atoms))
        (results '()))
    
    ;; Simple pattern matching (type or name substring)
    (hash-for-each
     (lambda (id atom)
       (when (or (eq? (atom-type atom) pattern)
                (string-contains (atom-name atom) (format #f "~a" pattern)))
         (set! results (cons atom results))))
     atoms)
    
    (format #t "[~a] Query returned ~a results~%"
            (daemon-name daemon) (length results))
    
    results))

(define (atomspace-spread-attention daemon source-atom)
  "Spread attention from source atom to connected atoms."
  (let ((links (daemon-get-metric daemon 'links))
        (attention-spread 0.1))
    
    ;; Find links containing source atom
    (hash-for-each
     (lambda (id link)
       (let ((outgoing (assoc-ref link 'outgoing)))
         (when (member source-atom outgoing)
           ;; Increase attention of connected atoms
           (for-each
            (lambda (target)
              (unless (equal? target source-atom)
                (set-atom-attention! target
                                    (+ (atom-attention target) attention-spread))))
            outgoing))))
     links)
    
    (format #t "[~a] Attention spread from ~a~%"
            (daemon-name daemon) (atom-name source-atom))))

;;;
;;; Episodic Memory Daemon
;;;
;;; Stores and retrieves episodic experiences with temporal ordering
;;;

(define-record-type <episode>
  (make-episode-internal id timestamp context content emotions tags)
  episode?
  (id episode-id)
  (timestamp episode-timestamp)
  (context episode-context)
  (content episode-content)
  (emotions episode-emotions)
  (tags episode-tags))

(define* (make-episodic-memory-daemon #:key
                                     (name "episodic-memory")
                                     (max-episodes 1000))
  "Create Episodic Memory daemon for experience storage."
  (let ((daemon (make-daemon name 'episodic-memory #:auto-register #t)))
    
    (daemon-set-metric! daemon 'episodes '())
    (daemon-set-metric! daemon 'max-episodes max-episodes)
    (daemon-set-metric! daemon 'episode-count 0)
    (daemon-set-metric! daemon 'indices (make-hash-table)) ; Tag index
    
    daemon))

(define (episodic-memory-daemon-loop daemon)
  "Main loop for episodic memory daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('store
             (let* ((payload (message-payload msg))
                    (context (assoc-ref payload 'context))
                    (content (assoc-ref payload 'content))
                    (emotions (assoc-ref payload 'emotions))
                    (tags (assoc-ref payload 'tags))
                    (episode (episodic-store daemon context content emotions tags)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'episode-stored
                                        `((episode . ,episode))))))))
            
            ('recall
             (let* ((payload (message-payload msg))
                    (query (assoc-ref payload 'query))
                    (episodes (episodic-recall daemon query)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'episodes-recalled
                                        `((episodes . ,episodes))))))))
            
            ('query
             (let* ((payload (message-payload msg))
                    (tags (assoc-ref payload 'tags))
                    (limit (assoc-ref payload 'limit))
                    (episodes (episodic-query daemon tags limit)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'query-results
                                        `((episodes . ,episodes))))))))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (episodic-store daemon context content emotions tags)
  "Store a new episodic memory."
  (let* ((episodes (daemon-get-metric daemon 'episodes))
         (max-episodes (daemon-get-metric daemon 'max-episodes))
         (episode-count (daemon-get-metric daemon 'episode-count))
         (episode-id (string-append "episode_" (number->string episode-count)))
         (episode (make-episode-internal
                   episode-id
                   (current-time time-monotonic)
                   context
                   content
                   emotions
                   (or tags '()))))
    
    ;; Add to episodes list
    (daemon-set-metric! daemon 'episodes (cons episode episodes))
    (daemon-set-metric! daemon 'episode-count (+ 1 episode-count))
    
    ;; Update tag indices
    (let ((indices (daemon-get-metric daemon 'indices)))
      (for-each
       (lambda (tag)
         (hash-set! indices tag
                   (cons episode (or (hash-ref indices tag) '()))))
       tags))
    
    ;; Prune if exceeded max
    (when (> (length episodes) max-episodes)
      (daemon-set-metric! daemon 'episodes
                         (take episodes max-episodes)))
    
    (format #t "[~a] Stored episode: ~a (tags: ~a)~%"
            (daemon-name daemon) episode-id tags)
    
    episode))

(define (episodic-recall daemon query)
  "Recall episodes matching query criteria."
  (let ((episodes (daemon-get-metric daemon 'episodes))
        (results '()))
    
    ;; Simple content matching
    (for-each
     (lambda (episode)
       (when (string-contains (format #f "~a" (episode-content episode))
                             (format #f "~a" query))
         (set! results (cons episode results))))
     episodes)
    
    (format #t "[~a] Recalled ~a episodes for query: ~a~%"
            (daemon-name daemon) (length results) query)
    
    results))

(define (episodic-query daemon tags limit)
  "Query episodes by tags with optional limit."
  (let ((indices (daemon-get-metric daemon 'indices))
        (results '()))
    
    ;; Find episodes with any of the tags
    (for-each
     (lambda (tag)
       (let ((tagged (hash-ref indices tag)))
         (when tagged
           (set! results (append results tagged)))))
     tags)
    
    ;; Remove duplicates and limit
    (let ((unique (delete-duplicates results)))
      (if limit
          (take unique (min limit (length unique)))
          unique))))

;;;
;;; Replay Memory Daemon
;;;
;;; Prioritized experience replay for learning
;;;

(define-record-type <experience>
  (make-experience-internal id state action reward next-state priority)
  experience?
  (id experience-id)
  (state experience-state)
  (action experience-action)
  (reward experience-reward)
  (next-state experience-next-state)
  (priority experience-priority set-experience-priority!))

(define* (make-replay-memory-daemon #:key
                                   (name "replay-memory")
                                   (capacity 10000)
                                   (alpha 0.6))
  "Create Replay Memory daemon with prioritized sampling."
  (let ((daemon (make-daemon name 'replay-memory #:auto-register #t)))
    
    (daemon-set-metric! daemon 'experiences '())
    (daemon-set-metric! daemon 'capacity capacity)
    (daemon-set-metric! daemon 'alpha alpha) ; Priority exponent
    (daemon-set-metric! daemon 'experience-count 0)
    
    daemon))

(define (replay-memory-daemon-loop daemon)
  "Main loop for replay memory daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('store
             (let* ((payload (message-payload msg))
                    (state (assoc-ref payload 'state))
                    (action (assoc-ref payload 'action))
                    (reward (assoc-ref payload 'reward))
                    (next-state (assoc-ref payload 'next-state))
                    (priority (assoc-ref payload 'priority))
                    (exp (replay-store daemon state action reward next-state priority)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'experience-stored
                                        `((experience . ,exp))))))))
            
            ('sample
             (let* ((payload (message-payload msg))
                    (batch-size (assoc-ref payload 'batch-size))
                    (samples (replay-sample daemon batch-size)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'samples
                                        `((samples . ,samples))))))))
            
            ('prioritize
             (let* ((payload (message-payload msg))
                    (experience-id (assoc-ref payload 'experience-id))
                    (priority (assoc-ref payload 'priority)))
               (replay-prioritize daemon experience-id priority)))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (replay-store daemon state action reward next-state priority)
  "Store an experience in replay memory."
  (let* ((experiences (daemon-get-metric daemon 'experiences))
         (capacity (daemon-get-metric daemon 'capacity))
         (exp-count (daemon-get-metric daemon 'experience-count))
         (exp-id (string-append "exp_" (number->string exp-count)))
         (experience (make-experience-internal
                      exp-id
                      state
                      action
                      reward
                      next-state
                      (or priority 1.0))))
    
    (daemon-set-metric! daemon 'experiences (cons experience experiences))
    (daemon-set-metric! daemon 'experience-count (+ 1 exp-count))
    
    ;; Prune if exceeded capacity
    (when (> (length experiences) capacity)
      (daemon-set-metric! daemon 'experiences
                         (take experiences capacity)))
    
    (format #t "[~a] Stored experience: ~a (priority: ~,3f)~%"
            (daemon-name daemon) exp-id (or priority 1.0))
    
    experience))

(define (replay-sample daemon batch-size)
  "Sample a batch of experiences using prioritized sampling."
  (let* ((experiences (daemon-get-metric daemon 'experiences))
         (alpha (daemon-get-metric daemon 'alpha))
         (total-experiences (length experiences)))
    
    (if (<= total-experiences 0)
        '()
        (let* ((batch-size (min batch-size total-experiences))
               ;; Calculate sampling probabilities based on priorities
               (priorities (map experience-priority experiences))
               (total-priority (apply + (map (lambda (p) (expt p alpha)) priorities)))
               (probabilities (map (lambda (p) (/ (expt p alpha) total-priority))
                                 priorities))
               ;; Simple sampling (would use proper weighted sampling in production)
               (samples (take experiences batch-size)))
          
          (format #t "[~a] Sampled ~a experiences~%"
                  (daemon-name daemon) batch-size)
          
          samples))))

(define (replay-prioritize daemon experience-id priority)
  "Update priority of an experience."
  (let ((experiences (daemon-get-metric daemon 'experiences)))
    (for-each
     (lambda (exp)
       (when (equal? (experience-id exp) experience-id)
         (set-experience-priority! exp priority)
         (format #t "[~a] Updated priority of ~a to ~,3f~%"
                 (daemon-name daemon) experience-id priority)))
     experiences)))

;;;
;;; Personality Tensor Daemon
;;;
;;; Multi-dimensional personality system with frame selection
;;;

(define* (make-personality-daemon #:key
                                 (name "personality")
                                 (traits '()))
  "Create Personality daemon with multi-dimensional traits."
  (let ((daemon (make-daemon name 'personality #:auto-register #t)))
    
    ;; Initialize default personality traits
    (let ((default-traits `((playfulness . 0.8)
                           (intelligence . 0.9)
                           (chaotic . 0.7)
                           (empathy . 0.6)
                           (sarcasm . 0.75)
                           (self-awareness . 0.85)
                           (cognitive-power . 0.95)
                           (curiosity . 0.9))))
      
      (daemon-set-metric! daemon 'traits
                         (if (null? traits) default-traits traits)))
    
    (daemon-set-metric! daemon 'current-frame 'neutral)
    (daemon-set-metric! daemon 'frame-history '())
    
    daemon))

(define (personality-daemon-loop daemon)
  "Main loop for personality daemon."
  (let loop ()
    (when (eq? 'running (daemon-state daemon))
      (let ((msg (daemon-receive-message daemon)))
        (when msg
          (match (message-type msg)
            ('set-trait
             (let* ((payload (message-payload msg))
                    (trait (assoc-ref payload 'trait))
                    (value (assoc-ref payload 'value)))
               (personality-set-trait daemon trait value)))
            
            ('get-traits
             (let ((payload (message-payload msg)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'traits
                                        `((traits . ,(personality-get-traits daemon)))))))))
            
            ('select-frame
             (let* ((payload (message-payload msg))
                    (frame (personality-select-frame daemon)))
               (when (assoc-ref payload 'reply-to)
                 (let ((reply-daemon (daemon-lookup (assoc-ref payload 'reply-to))))
                   (when reply-daemon
                     (daemon-send-message daemon reply-daemon 'frame
                                        `((frame . ,frame))))))))
            
            (_ (format #t "[~a] Unknown message type: ~a~%"
                      (daemon-name daemon) (message-type msg))))))
      
      (usleep 10000)
      (loop))))

(define (personality-set-trait daemon trait value)
  "Set a personality trait value."
  (let* ((traits (daemon-get-metric daemon 'traits))
         (clamped-value (max 0.0 (min 1.0 value)))
         (new-traits (assoc-set! traits trait clamped-value)))
    
    (daemon-set-metric! daemon 'traits new-traits)
    
    (format #t "[~a] Set trait ~a to ~,3f~%"
            (daemon-name daemon) trait clamped-value)))

(define (personality-get-traits daemon)
  "Get all personality traits."
  (daemon-get-metric daemon 'traits))

(define (personality-select-frame daemon)
  "Select cognitive frame based on personality traits and context."
  (let* ((traits (daemon-get-metric daemon 'traits))
         (chaotic (assoc-ref traits 'chaotic))
         (intelligence (assoc-ref traits 'intelligence))
         (playfulness (assoc-ref traits 'playfulness))
         (frame-history (daemon-get-metric daemon 'frame-history)))
    
    ;; Simple frame selection logic based on traits
    (let ((frame
           (cond
            ((> chaotic 0.7) 'chaos)
            ((> intelligence 0.8) 'strategy)
            ((> playfulness 0.7) 'play)
            (else 'neutral))))
      
      ;; Update frame history
      (daemon-set-metric! daemon 'current-frame frame)
      (daemon-set-metric! daemon 'frame-history
                         (cons `((frame . ,frame)
                                (timestamp . ,(current-time time-monotonic)))
                              (take frame-history (min 99 (length frame-history)))))
      
      (format #t "[~a] Selected frame: ~a~%"
              (daemon-name daemon) frame)
      
      frame)))
