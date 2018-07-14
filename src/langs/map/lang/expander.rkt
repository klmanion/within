#lang racket/base

(require (for-syntax racket/base
           syntax/parse
           racket/class))
(require syntax/parse
  racket/stxparam)
(module+ test
  (require rackunit rackunit/text-ui))

(provide #%top #%app #%datum)

;; map-expander-settings {{{
;

(define-for-syntax map-expander-settings
  (new
    (class object%
      (super-new)
      (field [room-height 80])
  
      ;; Accessor/Mutator member functions {{{
      ;
  
      ;; room-height {{{
      (define/public get-room-height
        (λ ()
          room-height))
  
      (define/public set-room-height!
        (λ (nv)
          (when (integer? nv)
            (set! room-height nv))))
      ;; }}}
  
      ;; }}}
)))

;; }}}

;; Syntax classes {{{
;
(begin-for-syntax
  (define-syntax-class clause
    #:literals (clause)
    (pattern (clause chead:clause-head cbody:clause-body)))
  
  (define-splicing-syntax-class clause-head
    #:literals (clause-head)
    (pattern (clause-head
               cname:clause-name
               (~optional id:str #:defaults ([id #'#f])))
      #:attr name (attribute cname.name)))
  
  (define-syntax-class clause-name
    #:literals (clause-name)
    (pattern (clause-name name)))
  
  (define-syntax-class clause-body
    #:literals (clause-body)
    (pattern (clause-body cbl:clause-body-line ...)))
  
  (define-syntax-class clause-body-line
    (pattern (:assignment))
    (pattern (:directive))
    (pattern (:clause)))
  
  (define-syntax-class assignment
    #:literals (assignment)
    (pattern (assignment member-id:id rval:rvalue)))
  
  (define-syntax-class rvalue
    #:literals (rvalue)
    (pattern (rvalue content)))
  
  (define-syntax-class directive
    #:literals (directive)
    (pattern (directive word:id))))

;; }}}

;; #%module-begin {{{
;

#|
(define-syntax old-map-module-begin
  (syntax-parser
    [(PARSE_TREE)
     #`(#%module-begin
        (module+ configure-runtime
          (require racket/class "ship.rkt" "room.rkt"))

        (begin-for-syntax
          (define room-ids
            (stx-map (λ (clause)
                       (syntax-rules ()
                         [(rc:room-clause) (attribute rc.id)]))
                     PARSE_TREE)))
        room-ids ; for DEBUG

        #,@(map (λ (id)
                  #'(define id (new room% [name id])))
                room-ids)

        (begin-for-syntax
          (define ship (new ship% [rooms room-ids])))

        PARSE_TREE
        (provide ship))]))
  |#

(define-syntax map-module-begin
  (syntax-parser
    [(_ PARSE-TREE)
     #'(#%module-begin
        (module+ configure-runtime
          (require racket/base racket/class "ship.rkt" "room.rkt"))
        PARSE-TREE)]))
(provide (rename-out [map-module-begin #%module-begin]))

;; }}}

;; Syntax expanders {{{
;

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
  (syntax-parser
    [(_ c:clause ...)
     #'(begin c ...)]))
(provide program)

(define-syntax clause
  (syntax-parser
    [(_ chead:clause-head cbody:clause-body)
     (syntax-parameterize
       ([current-container current-obj]
        [current-obj
         (if (eq? (attribute chead.name) "HEAD")
             map-expander-settings
             (attribute chead.id))])
        #'cbody)]))
(provide clause)

(define-syntax clause-body
  (syntax-parser
    [(cbl:clause-body-line ...) #'(begin cbl ...)]))
(provide clause-body)

(define-syntax assignment
  (syntax-parser
    [a:assignment
     #'(set-field! a.member-id current-obj a.rval)]))
(provide assignment)

(define-syntax directive
  (syntax-parser
    [(d:directive)
     #'(send current-obj d.word)]))
(provide directive)

;; }}}

;; Unit tests {{{
;

(module+ test
  (run-tests
    (test-suite "syntax expanders"
      )))
; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
