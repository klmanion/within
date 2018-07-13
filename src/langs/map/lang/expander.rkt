#lang racket/base

(require (for-syntax racket/base syntax/parse))
(require racket/class
  syntax/parse
  racket/stxparam)
(module+ test
  (require rackunit rackunit/text-ui))

(provide #%top #%app #%datum)

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

(define-syntax-class room-clause
  (pattern ({~literal room-clause} cname:str id cbody)))

;; }}}

;; #%module-begin {{{
;

(define-syntax old-map-module-begin
  (syntax-rules ()
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

(define-syntax map-module-begin
  (syntax-rules ()
    [(PARSE_TREE)
     #'(#%module-begin
        (module+ configure-runtime
          (require racket/class "ship.rkt" "room.rkt"))
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

(define-syntax real-program
  (syntax-rules ()
    [(_ clause ...)
     #'(begin clause ...)]))

(define-syntax program
  (syntax-rules ()
    [(_ clause ...)
     #'(printf "~a\n" "working")]))

(define-syntax head-clause
  (syntax-rules ()
    [(_ cname:str cbody)
     (syntax-parameterize ([in-head? #t]
                           [current-obj map-expander-settings])
       #'cbody)]))

(define-syntax clause-body
  (syntax-rules ()
    [(_ cbl ...) #'(begin cbl ...)]))

(define-syntax assignment
  (syntax-rules ()
    [(_ member-id rvalue)
     (unless (eq? current-obj #f)
       #`(send current-obj
               #,(id-mutator member-id)
               #,(syntax-e rvalue)))]))

(define-syntax directive
  (syntax-rules ()
    [(_ d)
     (unless (eq? current-obj #f)
       #'(send current-obj d))]))

(define-syntax entity-clause
  (syntax-rules ()
    [(_ cname cbody)
     (syntax-parameterize ([current-container current-obj])
       (syntax-parameterize ([current-obj (new (clause-header->class cname))])
         (begin #'cbody)))]))
         
;; }}}

;; Unit tests {{{
;

(module+ test
  (run-tests
    (test-suite "syntax expanders"
      )))
; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
