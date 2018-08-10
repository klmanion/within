#lang racket/base

(require racket/class
  racket/contract
  racket/function)
(module+ test
  (require rackunit
    rackunit/text-ui))

(provide parent<%> parent? parent/c child<%> child? child/c
  parent-mixin parent%
  child-mixin child%
  parent-child-mixin parent-child%)

(define parent<%>
  (interface ((class->interface object%))
    valid-child?
    add-child
    remove-child
    get-children
    get-first-child))

(define parent?
  (λ (o)
    (is-a? o parent<%>)))

(define parent/c
  (is-a?/c parent<%>))

(define child<%>
  (interface ((class->interface object%))
    valid-parent?
    has-valid-parent?
    orphan
    set-parent!
    add-to-parent
    get-parent))

(define child?
  (λ (o)
    (is-a? o child<%>)))

(define child/c
  (is-a?/c child<%>))

(define parent-mixin
  (mixin ((class->interface object%)) (parent<%>)
    (super-new)
    (init-field [children '()])

    (define/pubment valid-child?
      (λ (child)
        (and
          (child? child)
          (inner #t valid-child? child))))

    (define/public add-child
      (λ (child . bss)
        (when (valid-child? child)
          (unless (member child children)
            (set! children (cons child children))))
        (unless (null? bss)
          (add-child . bss))))

    (define/public remove-child
      (λ (child . bss)
        (set! children (remove* (list child) children))
        (unless (null? bss)
          (remove-child . bss))))

    (define/public get-children
      (λ ()
        children))

    (define/public get-first-child
      (λ ()
        (let ([children (get-children)])
          (if (null? children)
              children
              (car children)))))
))

(define/contract parent%
  (class/c
    (init-field [children (or/c null? (listof child/c))])
    [valid-child? (child/c . ->m . boolean?)]
    (inner [valid-child? (any/c . ->m . boolean?)])
    [add-child (->*m (child/c) () #:rest (listof child/c) any)]
    [remove-child (->*m (child/c) () #:rest (listof child/c) any)]
    get-children
    get-first-child)

  (parent-mixin object%))

(define child-mixin
  (mixin ((class->interface object%)) (child<%>)
    (super-new)
    (init-field [parent #f])

    ((thunk
       (add-to-parent)))

    (define/pubment valid-parent?
      (λ (parent)
        (and
          (parent? parent)
          (inner #t valid-parent? parent))))

    (define/public has-valid-parent?
      (λ ()
        (valid-parent? (get-parent))))

    (define/public add-to-parent
      (λ ()
        (unless (eq? parent #f)
          (when (parent? parent)
            (send parent add-child this)))))

    (define/public orphan
      (λ ()
        (unless (eq? parent #f)
          (send parent remove-child this)
          (set! parent #f))))

    (define/public set-parent!
      (λ (npar)
        (orphan)
        (set! parent npar)
        (add-to-parent)))

    (define/public get-parent
      (λ ()
        parent))
))

(define/contract child%
  (class/c
    (init-field [parent (or/c false/c parent/c)])
    [valid-parent? (any/c . ->m . boolean?)]
    [has-valid-parent? (->m boolean?)]
    [add-to-parent (->m any)]
    [orphan (->m any)]
    [set-parent! (parent/c . ->m . any)]
    get-parent)

  (child-mixin object%))

(define parent-child-mixin
  (λ (%)
    (parent-mixin (child-mixin %))))

(define/contract parent-child%
  (class/c
    (init-field [children (or/c null? (listof child/c))])
    [valid-child? (child/c . ->m . boolean?)]
    (inner [valid-child? (any/c . ->m . boolean?)])
    [add-child (->*m (child/c) () #:rest (listof child/c) any)]
    [remove-child (->*m (child/c) () #:rest (listof child/c) any)]
    get-children
    get-first-child

    (init-field [parent (or/c false/c parent/c)])
    [valid-parent? (any/c . ->m . boolean?)]
    [has-valid-parent? (->m boolean?)]
    [add-to-parent (->m any)]
    [orphan (->m any)]
    [set-parent! (parent/c . ->m . any)]
    get-parent)
  (parent-child-mixin object%))

;; Unit tests {{{
;

(module+ test
  (void (run-tests
    (test-suite "adding children"
      (test-suite "adding child to parent%"
        (test-case "adding child to parent% with add-to-parent"
          (define parent (new parent%))
          (define child (new child% [parent parent]))
          (send child add-to-parent)
          (check-equal?
            (send parent get-children)
            (list child)))
        (test-case "adding child to parent% implicitly"
          (define parent (new parent%))
          (define child (new child% [parent parent]))
          (check-equal?
            (send parent get-children)
            (list child))))
      (test-suite "adding child to parent% subclass"
        (test-case "adding child to subclass with add-to-parent"
          (define parent (new (class parent% (super-new))))
          (define child (new child% [parent parent]))
          (send child add-to-parent)
          (check-equal?
            (send parent get-children)
            (list child)))
        (test-case "adding child to subclass implicitly"
          (define parent (new (class parent% (super-new))))
          (define child (new child% [parent parent]))
          (check-equal?
            (send parent get-children)
            (list child))))))))

;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
