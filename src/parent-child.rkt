#lang racket/base

(require racket/class
  racket/function)

(provide parent<%> child<%>
  parent-mixin parent%
  child-mixin child%
  parent-child-mixin parent-child%)

(define parent<%>
  (interface ()
    valid-child?
    add-child
    remove-child
    get-children))

(define child<%>
  (interface ()
    orphan
    set-parent!))

(define parent-mixin
  (mixin () (parent<%>)
    (super-new)
    (init-field [children '()])

    (define/pubment valid-child?
      (λ (child)
        (and
          (is-a? child child<%>)
          (inner #t valid-child? child))))

    (define/public add-child
      (λ (child . bss)
        (when (valid-child? child)
          (unless (eq? (member child children) #f)
            (set! children (append child children))))
        (unless (null? bss)
          (add-child (car bss) (cdr bss)))))

    (define/public remove-child
      (λ (child . bss)
        (set! children (remove child children))
        (unless (null? bss)
          (remove-child (car bss) (cdr bss)))))

    (define/public get-children
      (λ ()
        children))
))

(define parent%
  (parent-mixin object%))

(define child-mixin
  (mixin () (child<%>)
    (super-new)
    (init-field [parent #f])

    ((thunk
       (add-to-parent)))

    (define/private add-to-parent
      (λ ()
        (unless (eq? parent #f)
          (when (is-a? parent parent<%>)
            (send parent add-child this)))))

    (define/public orphan
      (λ ()
        (unless (eq? parent #f)
          (send parent remove-child this)
          (set! parent #f))))

    (define/public set-parent!
      (λ (npar)
        (when (is-a? npar parent<%>)
          (orphan)
          (set! parent npar)
          (add-to-parent))))
))

(define child%
  (child-mixin object%))

(define parent-child-mixin
  (λ (%)
    (parent-mixin (child-mixin %))))

(define parent-child%
  (parent-child-mixin object%))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
