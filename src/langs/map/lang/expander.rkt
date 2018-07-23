#lang racket/base

(require (for-syntax racket/base
           racket/class
           syntax/parse
           racket/syntax
           racket/splicing
           syntax/id-table))
(require syntax/parse
  racket/stxparam
  racket/class
  xrepl)
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
      #:attr define (attribute chead.define)))

  (define lookup-table (make-free-id-table))

  (define free-id-table-has-key?
    (λ (tbl key)
      (not (eq? (member key (free-id-table-keys tbl)) #f))))
  
  (define-splicing-syntax-class clause-head
    #:literals (clause-head)
    (pattern (clause-head
               cname:clause-name
               (~optional id-str:str #:defaults ([id-str #'#f])))
      #:attr name (attribute cname.name)
      #:attr class (format-id (attribute name)
                              "~a%"
                              (string-downcase
                                (syntax-e (attribute name))))
      #:attr id (let* ([id-str-stx (attribute id-str)]
                       [id-str-val (syntax-e id-str-stx)])
                  (cond
                    [(string=? (syntax-e (attribute name)) "HEAD")
                     #'map-expander-settings]
                    [(string=? (syntax-e (attribute name)) "PARASITE")
                     #'parasite]
                    [(eq? id-str-val #f) #'#f]
                    [else (datum->syntax id-str-stx
                            (format-id id-str-stx
                                       "~a"
                                       id-str-val))]))
      #:attr id-val (if (eq? (syntax-e (attribute id)) #f)
                        #f
                        (format-id (attribute id)
                                   "~a"
                                   (syntax-e (attribute id))))
      #:attr define
        (unless (free-id-table-has-key? lookup-table (attribute id-val))
          (let ([cname (syntax-e (attribute name))])
            (begin0
              (cond
               [(string=? cname "HEAD")
                #'(void)]
               [(string=? cname "ROOM")
                #'(define id
                    (new class [parent (current-container)]
                               [room-name id-str]))]
               [(string=? cname "PARASITE")
                #'(define id
                    (new class [parent (current-container)]))]
               [(eq? (attribute id-val) #f)
                #'(new class [parent (current-container)])]
               [else
                #'(define id
                    (new class [parent (current-container)]))])
              (unless (eq? (attribute id-val) #f)
                (free-id-table-set! lookup-table (attribute id-val)
                                                 (attribute id))))))))
 
  (define-syntax-class clause-name
    #:literals (clause-name)
    (pattern (clause-name name)))
  
  (define-syntax-class clause-body
    #:literals (clause-body)
    (pattern (clause-body cbl:expr ...))
    (pattern (clause-body)))

  ;; replaced by `expr' in above syntax-class
  (define-syntax-class clause-body-line
    (pattern (:assignment))
    (pattern (:directive))
    (pattern (:clause)))
  
  (define-syntax-class assignment
    #:literals (assignment)
    (pattern (assignment lval:expr rval:expr)))

  (define-syntax-class lvalue
    #:literals (lvalue)
    (pattern (lvalue content-str:str)
      #:attr content (let* ([cnt-stx (attribute content-str)]
                            [cnt-val (syntax-e cnt-stx)])
                       (datum->syntax cnt-stx
                         (format-symbol "~a" cnt-val)))))
  
  (define-syntax-class rvalue
    #:literals (rvalue)
    (pattern (rvalue content)))
  
  (define-syntax-class directive
    #:literals (directive)
    (pattern (directive word-str:str)
      #:attr word (let* ([word-stx (attribute word-str)]
                         [word-val (syntax-e word-stx)])
                    (format-id word-stx "~a" word-val))))

  (define-syntax-class symbol
    #:literals (symbol)
    (pattern (symbol word:str)
      #:attr datum (let* ([word-stx (attribute word)]
                          [word-val (syntax-e word-stx)])
                     (datum->syntax word-stx
                       (format-symbol "~a" word-val))))))
;; }}}

;; Parameters {{{
;

(define current-obj (make-parameter #f))
(define current-container (make-parameter #f))

(provide current-obj current-container)

;; }}}

;; #%module-begin {{{
;

(require racket/contract/base)
(require "../../../defs.rkt")
(provide (all-from-out racket/class
           racket/contract/base))
(provide (all-from-out "../../../defs.rkt"))

(define-syntax map-module-begin
  (syntax-parser
    [(_ PARSE-TREE)
     #'(#%module-begin
        (define ship (new ship%))
        (parameterize ([current-obj ship])
          PARSE-TREE)
        (send ship place-rooms)
        (provide (contract-out [ship ship?])))]))
(provide (rename-out [map-module-begin #%module-begin]))
;; }}}

;; Syntax expanders {{{
;

(define-syntax program
  (syntax-parser
    [(_ c:clause ...)
     #'(begin
         (parameterize ([current-container (current-obj)])
           c.define ... ; this defines top-level room clauses
           c ...))]))
(provide program)

(define-syntax clause
  (syntax-parser
    [(_ chead:clause-head cbody:clause-body)
     #'(parameterize ([current-container (current-obj)])
         chead.define ; will not define if id is already bound
         (parameterize ([current-obj chead.id])
           cbody))]))
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
    [(_ cbl:expr ...) #'(begin cbl ... (void))]
    [_ #'(void)]))
(provide clause-body)

(define-syntax assignment
  (syntax-parser
    [a:assignment
     #'(dynamic-set-field! a.lval (current-obj) a.rval)]))
(provide assignment)

(define-syntax lvalue
  (syntax-parser
    [lval:lvalue #''lval.content]))
(provide lvalue)

(define-syntax rvalue
  (syntax-parser
    [rval:rvalue #'rval.content]))
(provide rvalue)

(define-syntax directive
  (syntax-parser
    [d:directive #'(send (current-obj) d.word)]))
(provide directive)

(define-syntax symbol
  (syntax-parser
    [sym:symbol #''sym.datum]))
(provide symbol)
;; }}}

;; Unit tests {{{
;

(module+ test
  (void (run-tests
    (test-suite "syntax expanders"
      ))))
; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
