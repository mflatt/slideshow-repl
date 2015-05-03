#lang racket
(require racket/gui
         framework
         errortrace/errortrace-key)

(provide slide:text%
         repl-text%
         set-font!)

(define slide-style-list
  (new style-list%))

;; Copy the standard style list:
(for ([i (in-range (send (editor:get-standard-style-list) number))])
  (send slide-style-list
        convert
        (send (editor:get-standard-style-list)
              index-to-style
              i)))

(define (set-font! sz font bold?)
  (define face
    (if (string? font)
        font
        (send the-font-name-directory
              get-screen-name
              (send the-font-name-directory find-family-default-font-id font)
              'normal
              'normal)))
  (define s (send slide-style-list find-named-style "Standard"))
  (define d (new style-delta%))
  (send s get-delta d)
  (send d set-delta 'change-size sz)
  (send d set-face face)
  (when bold?
    (send d set-weight-on 'bold) ; Unfortunately, syntax coloring seems to override this, so...
    (send d set-face (~a face ", bold"))) ; ... set face explciitly to a "bold" variant
  (send d set-family 'modern)
  (send s set-delta d))

(define (slide:style-list-mixin e%)
  (class e%
    (inherit set-style-list)
    (super-new)
    (set-style-list slide-style-list)))


;; like `racket:text%', but omits the standard-style mixin.
;; and enables undo history:
(define slide:text%
  (class (racket:set-mode-mixin
          (racket:text-mixin
           (text:autocomplete-mixin
            (mode:host-text-mixin 
             (color:text-mixin
              (editor:keymap-mixin
               (slide:style-list-mixin
                text:wide-snip%)))))))
    (super-new)
    (inherit set-max-undo-history)
    (set-max-undo-history 'forever)))

(define repl-text%
  (class (text:ports-mixin slide:text%)
    (inherit get-unread-start-point
             position-paragraph
             paragraph-start-position
             begin-edit-sequence
             end-edit-sequence
             reset-regions
             get-regions
             insert-between
             in-edit-sequence?
             send-eof-to-in-port
             get-in-port
             get-out-port
             get-err-port
             get-value-port
             get-start-position
             get-end-position
             get-character
             highlight-range)

    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (cond
       [(in-edit-sequence?) 
        (set! had-an-insert (cons start had-an-insert))]
       [else (update-after-inserts (list start))]))
    
    ;; private field
    (define had-an-insert '())
    
    (define/augment (after-edit-sequence)
      (inner (void) after-edit-sequence)
      (unless (null? had-an-insert)
        (let ([to-clean had-an-insert])
          (set! had-an-insert '())
          (update-after-inserts to-clean))))
    
    (define/private (update-after-inserts starts)
      (when (and prompt-position 
                 (ormap (λ (start) (< start prompt-position))
                        starts))
        (set! prompt-position (get-unread-start-point))
        (reset-regions (append (drop-right (get-regions) 1)
                               (list (list prompt-position 'end))))))

    (define prompt-position #f)

    (define inserting-prompt? #f)
    (define/public (get-prompt) "> ")
    (define/public (insert-prompt)
      (set! inserting-prompt? #t)
      (begin-edit-sequence)
      (let* ([pmt (get-prompt)]
             [prompt-space (string-length pmt)])
        
        ;; insert the prompt, possibly inserting a newline first
        (let* ([usp (get-unread-start-point)]
               [usp-para (position-paragraph usp)]
               [usp-para-start (paragraph-start-position usp-para)])
          (unless (equal? usp usp-para-start)
            (insert-between "\n")
            (set! prompt-space (+ prompt-space 1)))
          (insert-between pmt))
        
        (let ([sp (get-unread-start-point)])
          (set! prompt-position sp)
          (reset-regions (append (get-regions) (list (list sp 'end))))))
      (end-edit-sequence)
      (set! inserting-prompt? #f))

    (inherit clear-input-port
             clear-box-input-port
             clear-output-ports
             set-allow-edits
             reset-input-box
             lock
             delete
             erase
             last-position
             set-position
             set-unread-start-point
             set-insertion-point)

    (define indenting-limit 0)
    (define/override (get-limit n) 
      (cond
       [(< n indenting-limit) 0]
       [else indenting-limit]))

    (define/public (reset-console)
      (clear-input-port)
      (clear-box-input-port)
      (clear-output-ports)
      (set-allow-edits #t)
      
      ;; in case the last evaluation thread was killed, clean up some state.
      (lock #f)
      
      ;; clear out repl first before doing any work.
      (begin-edit-sequence)
      (set! prompt-position #f)
      (reset-input-box)
      (erase)
      (end-edit-sequence)
      
      (begin-edit-sequence)
      (set-position (last-position) (last-position))
      
      (reset-regions (list (list (last-position) (last-position))))
      (set-unread-start-point (last-position))
      (set-insertion-point (last-position))
      (set! indenting-limit (last-position))
      (set-allow-edits #f)
      (end-edit-sequence))

    (define/augment (on-submit)
      (inner (void) on-submit)
      (let* ([old-regions (get-regions)]
             [abl (drop-right old-regions 1)]
             [lst (last old-regions)])
        (reset-regions (append abl (list (list (list-ref lst 0) (last-position))))))
      
      ;; lets us know we are done with this one interaction
      ;; (since there may be multiple expressions at the prompt)
      (send-eof-to-in-port)
      
      (set! prompt-position #f)
      (evaluate-from-port
       (get-in-port) 
       #f
       (λ ()
          ;; clear out the eof object if it wasn't consumed
          (clear-input-port))))

    (define/augment (submit-to-port? key)
      (or (eq? (send key get-key-code) 'numpad-enter)
          (send key get-control-down)
          (send key get-alt-down)
          (and prompt-position
               (and 
                (only-whitespace-after-insertion-point)
                (let ([p (open-input-text-editor this prompt-position)])
                  (let loop ()
                    (with-handlers* ([exn:fail:read:eof? (lambda (x) #f)]
                                     [exn:fail:read? (lambda (x) (loop))])
                      (or (eof-object? (read p))
                          (loop)))))))))

    (define/private (only-whitespace-after-insertion-point)
      (let ([start (get-start-position)]
            [end (get-end-position)])
        (and (= start end)
             (let loop ([pos start])
               (cond
                [(= pos (last-position)) #t]
                [else (and (char-whitespace? (get-character pos))
                           (loop (+ pos 1)))])))))

    (init [(init-ns namespace)])
    (define ns init-ns)
    
    (define/public (call-in-eval thunk) (void))

    (define/public (evaluate-from-port port ??? callback)
      (set! need-interaction-cleanup? #t)
      (call-in-eval
       (lambda ()
         (let loop ()
           (define e (read-syntax port port))
           (unless (eof-object? e)
             (with-handlers ([exn? (lambda (exn)
                                     (show-error exn))])
               (call-with-values (lambda () (eval e ns))
                 (lambda l (map (current-print) l))))
             (loop)))))
      (cleanup)
      (yield
       (thread
        (lambda ()
          (flush-output (get-value-port)))))
      (callback)
      (cleanup-interaction)
      (insert-prompt))

    (define/public (show-error exn)
      (define locs (if (exn:srclocs? exn)
                       ((exn:srclocs-accessor exn) exn)
                       (let ([l (cms->srclocs (exn-continuation-marks exn))])
                         (if (pair? l)
                             (list (car l))
                             null))))
      (for ([loc (in-list locs)])
        (define where (srcloc-source loc))
        (when (where . is-a? . text%)
          (define start (srcloc-position loc))
          (when (and start (srcloc-span loc))
            (define finish (+ start (srcloc-span loc)))
            (send where highlight-range (sub1 start) (sub1 finish) "pink" #f 'high))))
      (write-string (exn-message exn)
                    (get-err-port))
      (newline (get-err-port)))

    (define/private (cleanup) (void))

    (define need-interaction-cleanup? #f)

    (define/private (cleanup-interaction)
      (set! need-interaction-cleanup? #f)
      (begin-edit-sequence)
      (set-caret-owner #f 'display)
      (cleanup)
      (end-edit-sequence))

    (inherit freeze-colorer thaw-colorer clear-undos
             set-caret-owner)

    (define/public (initialize-console)
      (begin-edit-sequence)
      (freeze-colorer)
      (reset-console)
      (thaw-colorer)
      (insert-prompt)
      (end-edit-sequence)
      (clear-undos))

    (inherit get-backward-sexp split-snip
             get-snip-position insert
             find-snip)
    (define/override (on-local-char key)
      (let ([start (get-start-position)]
            [end (get-end-position)]
            [code (send key get-key-code)])
        (cond
         [(not (or (eq? code 'numpad-enter)
                   (equal? code #\return)
                   (equal? code #\newline)))
          (super on-local-char key)]
         [(not prompt-position) 
          ;; evaluating? just drop the keypress
          (void)] 
         [(and (< end prompt-position)
               (= start end)
               (get-backward-sexp end))
          =>
          (λ (sexp-start)
             (copy-down sexp-start end))]
         [(and (< end prompt-position)
               (not (= start end)))
          (copy-down start end)]
         [else
          (super on-local-char key)])))

    (define/private (copy-down start end)
      (begin-edit-sequence)
      (split-snip start)
      (split-snip end)
      (let loop ([snip (find-snip start 'after-or-none)])
        (when snip
          (let ([pos (+ (get-snip-position snip)
                        (send snip get-count))])
            (when (<= pos end)
              (insert (send snip copy) (last-position) (last-position))
              (loop (send snip next))))))
      (set-position (last-position) (last-position))
      (end-edit-sequence))

    (super-new)

    (initialize-console)))

;; cms->srclocs : continuation-marks -> (listof srcloc)
(define (cms->srclocs cms)
  (map 
   (λ (x) (make-srcloc (list-ref x 1)
                       (list-ref x 2)
                       (list-ref x 3)
                       (list-ref x 4)
                       (list-ref x 5)))
   (continuation-mark-set->list cms errortrace-key)))

(module+ main
  (define f (new frame%
                 [label "Test"]
                 [width 800]
                 [height 600]))
  (define t (new repl-text%))
  (void
   (new editor-canvas%
        [parent f]
        [editor t]))
  (send f show #t))
