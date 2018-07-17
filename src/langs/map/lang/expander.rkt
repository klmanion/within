#lang racket/base

(require (for-syntax racket/base
           racket/class
           syntax/parse
           racket/syntax))
(require syntax/parse
  racket/stxparam
  racket/class)
(module+ test
  (require rackunit rackunit/text-ui))

(provide #%top #%app #%datum #%top-interaction)

;; map-expander-settings {{{
;

(define map-expander-settings
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
    (pattern (clause chead:clause-head cbody:clause-body)
      #:attr define
        (let ([cname (syntax-e (attribute chead.name))])
          (cond
           [(string=? cname "HEAD")
            #'(void)]
           [(string=? cname "ROOM")
            #'(define chead.id
                (new chead.class [parent (current-container)]
                                 [name chead.id-str]))]
           [(string=? cname "PARASITE")
            #'(define parasite
                (new chead.class [parent (current-container)]))]
           [(eq? (syntax-e (attribute chead.id)) #f)
            #'(new chead.class [parent (current-container)])]
           [else
            #'(define chead.id
                (new chead.class))]))))
  
  (define-splicing-syntax-class clause-head
    #:literals (clause-head)
    (pattern (clause-head
               cname:clause-name
               (~optional id-str:str
                 #:defaults ([id-str #'#f])))
      #:attr name (attribute cname.name)
      #:attr class (format-id (attribute name)
                              "~a%"
                              (string-downcase
                                (syntax-e (attribute name))))
      #:attr id (let* ([id-str-stx (attribute id-str)]
                       [id-str-val (syntax-e id-str-stx)])
                  (cond
                    [(eq? id-str-val #f) #'#f]
                    [else (format-id id-str-stx
                                     "~a"
                                     id-str-val)]))))
 
  (define-syntax-class clause-name
    #:literals (clause-name)
    (pattern (clause-name name)))
  
  (define-syntax-class clause-body
    #:literals (clause-body)
    (pattern (clause-body cbl:expr ...)))
  
  ;; replaced by `expr' in above syntax-class
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
    (pattern (directive word))))

;; }}}

;; Parameters {{{
;

(define current-obj (make-parameter #f))
(define current-container (make-parameter #f))

(provide current-obj current-container)

;; }}}

;; }}}

;; #%module-begin {{{
;

(require "../../../ship.rkt"
  "../../../room.rkt"
  "../../../parasite.rkt")
(provide (all-from-out "../../../ship.rkt"
           "../../../room.rkt"
           "../../../parasite.rkt"))

(define-syntax map-module-begin
  (syntax-parser
    [(_ PARSE-TREE)
     #'(#%module-begin
        (require racket/base racket/class)

        (define ship (new ship%))
        (parameterize ([current-obj ship])
          PARSE-TREE)
        (provide ship))]))
(provide (rename-out [map-module-begin #%module-begin]))

;; }}}

;; Syntax expanders {{{
;

(define-syntax program
  (syntax-parser
    [(_ c:clause ...)
     #'(begin
         (parameterize ([current-container (current-obj)])
           c.define ... (void))
         c ...)]))
(provide program)

(define-syntax clause
  (syntax-parser
    [(_ chead:clause-head cbody:clause-body)
     #'(parameterize ([current-container (current-obj)]
                      [current-obj (if (string=? chead.name "HEAD")
                                       map-expander-settings
                                       chead.id)])
         cbody)]))
(provide clause)

;; this silences an `unbound-literal' error for the syntax class
(define-syntax clause-head
  (syntax-rules ()))
(provide clause-head)

(define-syntax clause-name
  (syntax-rules ()))
(provide clause-name)

(define-syntax clause-body
  (syntax-parser
    [(cbl:expr ...) #'(begin cbl ...)]))
(provide clause-body)

(define-syntax assignment
  (syntax-parser
    [a:assignment
     #'(set-field! a.member-id (current-obj) a.rval)]))
(provide assignment)

(define-syntax directive
  (syntax-parser
    [d:directive
     #'(send (current-obj) d.word)]))
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
