#lang racket/base
(require (for-syntax racket/base syntax/parse))

;; #%module-begin {{{
;

(define-syntax map-module-begin
  (syntax-rules ()
    [(_ PARSE_TREE)
     #'(#%module-begin
        'PARSE_TREE)]))
(provide (rename-out [map-module-begin #%module-begin]))
;; }}}

;; Syntax expanders {{{
;

(define-syntax-parameter in-head? #f)
(define-syntax-parameter current-obj #f)
(define-syntax-parameter current-container #f)

;; Helper functions {{{
;

(define-for-syntax clause-head->id
  (syntax-rules ()
    [(_ nm id) id]))

(define-for-syntax room-clause->define
  (λ (rc-stx . bss)
    (cond
      [(null? rc-stx) (void)]
      [else
       (syntax-parse rc-stx
         [(_ rch rcb ...)
          #'((begin
               (define (clause-head->id rch) (new room%))
               #,(unless (null? bss)
                   (room-clause->define (car bss) (cdr bss)))))])])))

(define-syntax id-mutator
  (syntax-rules ()
    [(id)
     (datum->syntax (string-append "set-" (syntax->datum id) "!"))]))

(define-syntax clause-header->class
  (syntax-rules ()
    [(_ cn _)
     (datum->syntax
       (string-append (string-downcase (syntax-e cn))
                      "%"))]))
;; }}}

(define-syntax program
  (syntax-rules ()
    [(_ head-clause clause ...)
     #`((begin
          head-clause
          #,(room-clause->define clause ...)
          clause ...))]))

(define-syntax head-clause
  (syntax-rules ()
    [(_ clause)
     (syntax-parameterize ([in-head? #t])
       #'clause)]))

(define-syntax clause
  (syntax-rules ()
    [(_ ch cb)
     (syntax-parameterize ([current-obj
                            (if in-head? 
                                #f
                                (new (clause-header->class ch)))])
       #'((begin
            cb
            (unless (eq? current-container #f)
              (send current-container add-entity current-obj)))))]))

(define-syntax id
  (syntax-rules ()
    [(_ w) #'w]))

(define-syntax clause-body
  (syntax-rules ()
    [(_ ...) #'((begin ...))]))

(define-syntax assignment
  (syntax-rules ()
    [(_ member-id rvalue)
     (unless (eq? current-obj #f)
       #`((send current-obj
                #,(id-mutator member-id)
                #,(syntax-e rvalue))))]))

(define-syntax directive
  (syntax-rules ()
    [(_ d)
     (unless (eq? current-obj #f)
       #'((send current-obj d)))]))

(define-syntax contained
  (syntax-rules ()
    [(_ c)
     (syntax-parameterize ([current-container current-obj])
       #'c)]))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
