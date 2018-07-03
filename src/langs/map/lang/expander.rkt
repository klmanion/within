#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-syntax map-module-begin
  (syntax-rules ()
    [(_ PARSE_TREE)
     #'(#%module-begin
        (define data PARSE_TREE)
        (provide data))]))
(provide (rename-out [map-module-begin #%module-begin]))

(define-syntax-parameter current-obj #f)
(define-syntax-parameter current-container #f)

(define-for-syntax clause-head->id
  (syntax-rules ()
    [(_ _ id _) id]
    [_ '()]))

(define-for-syntax room-clause->define
  (λ (rc-stx . bss)
    (cond
      [(null? rc-stx) (void)]
      [else
       (syntax-parse rc-stx
         [(_ rch rcb ...)
          #'(define (clause-head->id rch) new room%
            #,(unless (null? bss)
                (room-clause->define (car bss) (cdr bss))))])])))

(define-syntax program
  (syntax-rules ()
    [(_ head-clause room-clause ...)
     #`(begin
         head-clause
         #,(room-clause->define room-clause ...)
         ...)]))

(define-syntax head-clause
  (syntax-rules ()
    [(_ hch hcb ...)
     #'(begin hcb ...)]))

(define-syntax room-clause
  (syntax-rules ()
    [(_ rch rcb ...)
     (syntax-parameterize ([current-obj (clause-head->id rch)])
       #'(begin rcb ...))]))

(define-syntax room-clause-body
  (syntax-rules ()
    [(_ clause-body) #'(clause-body)]))

(define-syntax clause-body
  (syntax-rules ()
    [(_ ...) #'(...)]))

(define-syntax mutator
  (syntax-rules ()
    [(id)
     (datum->syntax (string-append "set-" (syntax->datum id) "!"))]))

(define-syntax assignment
  (syntax-rules ()
    [(_ member-id rvalue)
     (unless (eq? current-obj #f)
       #`(send current-obj
               #,(mutator member-id)
               #,(syntax-e rvalue)))]))

(define-syntax directive
  (syntax-rules ()
    [(_ d)
     (unless (eq? current-obj #f)
       #'(send current-obj d))]))

(define-syntax contained
  (syntax-rules ()
    [(_ c)
     (syntax-parameterize ([current-container current-obj])
       #'(c))]))

(define-syntax clause-header->class
  (syntax-rules ()
    [(_ cn _)
     (datum->syntax
       (string-append (string-downcase (syntax-e cn))
                      "%"))]))

(define-syntax clause
  (syntax-rules ()
    [(_ ch cb ...)
     (syntax-parameterize ([current-obj (new (clause-header->class ch))])
       #'(begin
           cb ...
           (send current-container add-entity current-obj)))]))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
