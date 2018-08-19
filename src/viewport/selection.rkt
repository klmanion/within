#lang racket/base
(require racket/class
  racket/contract
  racket/function)
(require "../entity.rkt")

(provide selection<%> selection? selection/c selection%)

;; selection% {{{
;
(define selection<%>
  (interface ((class->interface object%))
    get-selection
    set-selection!
    add-entity!
    append-list!
    empty?
    clear
    send-members))

(define selection?
  (λ (o)
    (is-a? o selection<%>)))

(define selection/c
  (is-a?/c selection<%>))

(define/contract selection%
  (class/c
    (init-field [lst (or/c null? (listof entity/c))])
    get-selection
    
    [set-selection! ((or/c null? (listof entity/c)) . ->m . any)]
    [add-entity! (entity/c . ->m . any)]
    [append-list! ((or/c null? (listof entity/c)) . ->m . any)]

    [empty? (->m any)]

    [clear (->m any)]
    
    [send-members (->*m (symbol?) () #:rest (listof any/c) any)])

  (class* object% (selection<%>)
    (super-new)
    (init-field [lst '()])

    ;; Accessor methods {{{
    ;
    (define/public get-selection
      (λ ()
        lst))
    ;; }}}

    ;; Mutator methods {{{
    ;
    (define/public set-selection!
      (λ (nlst)
        (set! lst nlst)))

    (define/public add-entity!
      (λ (e)
        (set-selection! (cons e (get-selection)))))

    (define/public append-list!
      (λ (nlst)
        (if (null? (get-selection))
            (set-selection! nlst)
            (set-selection! (append nlst (get-selection))))))
    ;; }}}

    ;; Predicates {{{
    ;
    (define/public empty?
      (λ ()
        (empty? (get-selection))))
    ;; }}}

    ;; Action methods {{{
    ;
    (define/public clear
      (λ ()
        (set-selection! '())))

    (define/public send-members
      (λ (proc . rst)
        (for-each
          (λ (o)
            (apply dynamic-send o proc rst))
          (get-selection))))
    ;; }}}
))
;; }}}
