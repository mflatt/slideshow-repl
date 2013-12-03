#lang racket/base
(require slideshow
         slideshow/code
         racket/gui/base
         framework
         "private/editor.rkt"
         syntax/modread
         racket/date)

(provide
 repl-group?
 module-backing?
 (contract-out
  [make-repl-group (->* ()
                        (#:log-file path-string?
                                    #:prompt (or/c #f string?))
                        repl-group?)]
  [make-module-backing (->* (repl-group?)
                            (#:module-name path-string?)
                            #:rest (listof string?)
                            module-backing?)]
  [module-backing-module-name (-> module-backing?
                                  path-string?)]
  [module-area (->* (module-backing?)
                    (#:width real?
                             #:height real?
                             #:background (or/c #f (is-a?/c color%) string?)
                             #:font-size (or/c #f (integer-in 1 1024))
                             #:auto-eval? any/c)
                    pict?)]
  [result-area (->* (repl-group?)
                    (#:width real?
                             #:height real?
                             #:background (or/c #f (is-a?/c color%) string?)
                             #:font-size (or/c #f (integer-in 1 1024)))
                    #:rest (listof string?)
                    pict?)]
  [repl-area (->* ()
                  (#:width real?
                             #:height real?
                             #:background (or/c #f (is-a?/c color%) string?)
                             #:font-size (or/c #f (integer-in 1 1024))
                             #:prompt string?)
                  #:rest (listof string?)
                  pict?)]))

(define (scale-font-size s)
  (inexact->exact (floor (* (car (current-expected-text-scale)) s))))

(define default-font-size (scale-font-size (current-font-size)))

(define on-screen 0)

(struct repl-group (module-area-maker result-area))
(struct module-backing (area module-name))

(define (install-background c background)
  (when background
    (send c set-canvas-background
          (if (string? background)
              (make-object color% background)
              background))))

(define (make-repl-group #:log-file [log-file "eval-log.rktl"]
                         #:prompt [prompt-str #f])
  (define result-editor 
    (new (class repl-text%
           (define/override (get-prompt) (or prompt-str ""))
           (super-new))))
  (define result-custodian (make-custodian))

  (define available (make-hash))
  (define (register-available! name t)
    (hash-set! available name t))
  (define (unregister-available! name)
    (hash-remove! available name))
  (define current-loading-modules (make-parameter null))

  (define (reset-font! font-size)
    (when (or font-size
              (zero? on-screen))
      (set-font! (or font-size default-font-size)
                 ;; face/family
                 (let loop ([l (current-code-font)])
                   (if (pair? l) (loop (cdr l)) l))
                 ;; bold?
                 (let loop ([l (current-code-font)])
                   (and (pair? l) 
                        (or (eq? 'bold (car l)) 
                            (loop (cdr l))))))))

  (define (do-eval name t sm)
    (custodian-shutdown-all result-custodian)
    (set! result-custodian (make-custodian))
    (send result-editor reset-console)
    (when prompt-str
      (send result-editor initialize-console))
    (for ([e (in-hash-values available)])
      (send e unhighlight-ranges void))

    (define ns (make-base-namespace))
    (namespace-attach-module (current-namespace) 'errortrace/errortrace-key ns)
    
    (yield
     (parameterize ([current-custodian result-custodian]
                    [current-namespace ns]
                    [current-module-name-resolver
                     (let ([orig (current-module-name-resolver)])
                       (case-lambda
                        [(a b) (orig a b)]
                        [(path rel-to stx load?)
                         (define t (and (string? path)
                                        (hash-ref available (path->complete-path path) #f)))
                         (define rp (and t (make-resolved-module-path
                                            (path->complete-path path))))
                         (if (and rp (not (module-declared? rp)))
                             (let ([loading (current-loading-modules)]
                                   [name (resolved-module-path-name rp)])
                               (when (member name loading)
                                 (error "cycle in loading modules"))
                               (parameterize ([current-loading-modules
                                               (cons name (current-loading-modules))])
                                 (do-one-eval name t))
                               rp)
                             (orig path rel-to stx load?))]))]
                    [current-output-port
                     (send result-editor get-value-port)]
                    [current-error-port
                     (send result-editor get-err-port)]
                    [exit-handler (lambda (v)
                                    (custodian-shutdown-all result-custodian))])
       (thread
        (lambda ()
          (dynamic-require 'errortrace #f)
          (with-handlers ([exn:fail? (lambda (x)
                                       (send result-editor
                                             show-error
                                             x))])
            (do-one-eval name t)
            (dynamic-require (if sm `(submod ,name ,sm) name) 0)
            (flush-output (send result-editor get-value-port))))))))

  (define (do-one-eval name t)
    (define txt (send t get-text))
    (call-with-output-file log-file
      #:exists 'append
      (lambda (o)
        (fprintf o ";; @ ~a -----------\n"
                 (date->string (seconds->date (current-seconds))
                               #t))
        (fprintf o ";; ~a\n" name)
        (display txt o)
        (newline o)))
    (define p (open-input-text-editor t #:lock-while-reading? #t))
    (port-count-lines! p)
    (parameterize ([error-print-source-location #f])
      (define e
        (with-module-reading-parameterization
         (lambda ()
           (parameterize ([read-accept-reader #t])
             (define e (read-syntax t p))
             (when (or (eof-object? e)
                       (not (eof-object? (read p))))
               (error "bad module"))
             (check-module-form e
                                'module
                                "module")))))
      (parameterize ([current-module-declare-name
                      (make-resolved-module-path name)])
        (eval e))))

  (define (module-area-maker #:name [mod-file-name #f]
                             init-lines)
    (define t (new (class slide:text%
                     (inherit unhighlight-ranges)
                     (define/private (clear-highlights)
                       (unhighlight-ranges void))
                     (define/augment (on-insert s l)
                       (clear-highlights)
                       (inner (void) on-insert s l))
                     (define/augment (on-delete s e)
                       (clear-highlights)
                       (inner (void) on-delete s e))
                     (super-new))))
    (for ([s (in-list init-lines)]
          [i (in-naturals)])
      (send t insert s)
      (unless (= (add1 i) (length init-lines))
        (send t insert "\n")))
    (define km (send t get-keymap))
    (define ((create font-size auto-eval? background) win)
      (send result-editor reset-console)
      (when prompt-str
        (send result-editor initialize-console))
      (reset-font! font-size)
      (define name (path->complete-path mod-file-name))
      (send km add-function "run"
            (lambda (v e)
              (do-eval name t #f)))
      (send km add-function "run-test"
            (lambda (v e)
              (do-eval name t 'test)))
      (send km map-function "f5" "run")
      (send km map-function "f6" "run-test")
      (define c 
        (new editor-canvas% 
             [parent win]
             [editor t]
             [style '(auto-vscroll auto-hscroll no-border)]))
      (install-background c background)
      (register-available! name t)
      (when auto-eval?
        (queue-callback (lambda ()
                          (do-eval name t #f))
                        #f))
      (set! on-screen (add1 on-screen))
      (lambda ()
        (set! on-screen (sub1 on-screen))
        (send c set-editor #f)
        (unregister-available! name)))
    (lambda (#:width w #:height h
                     #:background background
                     #:font-size font-size
                     #:auto-eval? auto-eval?)
      (define content (interactive (blank w h)
                                   (create font-size
                                           auto-eval?
                                           background)))
      content))

  (define (result-area #:width w
                       #:height h
                       #:font-size font-size
                       #:background background
                       content)
    (interactive (blank w h)
                 (lambda (win)
                   (reset-font! font-size)
                   (send result-editor reset-console)
                   (when prompt-str
                     (send result-editor initialize-console))
                   (unless (null? content)
                     (for ([c (in-list content)]
                           [i (in-naturals)])
                       (unless (zero? i)
                         (send result-editor insert "\n"))
                       (send result-editor insert c)))
                   (define c 
                     (new editor-canvas% 
                          [parent win]
                          [editor result-editor]
                          [style '(auto-vscroll auto-hscroll no-border)]))
                   (set! on-screen (add1 on-screen))
                   (install-background c background)
                   (lambda ()
                     (set! on-screen (sub1 on-screen))
                     (send c set-editor #f)))))


  (repl-group module-area-maker result-area))

(define (make-module-backing group
                             #:module-name [file-name "program.rkt"]
                             . content-lines)
  (module-backing ((repl-group-module-area-maker group)
                   #:name file-name
                   content-lines)
                  file-name))

(define (module-area backing
                     #:width [w (* client-w 1/4)]
                     #:height [h (* client-h 1/4)]
                     #:background [background #f]
                     #:font-size [font-size #f]
                     #:auto-eval? [auto-eval? #f])
  ((module-backing-area backing)
   #:width w 
   #:height h 
   #:background background
   #:font-size font-size
   #:auto-eval? auto-eval?))

(define (result-area group
                     #:width [w (* client-w 2/3)]
                     #:height [h (* client-h 1/4)]
                     #:background [background #f]
                     #:font-size [font-size #f]
                     . content)
  
  ((repl-group-result-area group)
   content
   #:width w
   #:height h
   #:font-size font-size
   #:background background))

(define (repl-area #:width [w (* client-w 2/3)]
                   #:height [h (* client-h 1/4)]
                   #:font-size [font-size #f]
                   #:background [background #f]
                   #:prompt [prompt-str "> "]
                   . content)
  (apply result-area (make-repl-group #:prompt prompt-str)
         #:width w
         #:height h
         #:font-size font-size
         #:background background
         content))

