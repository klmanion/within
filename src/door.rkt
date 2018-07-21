#lang racket/base

(require racket/class
  racket/gui/base)
(require "entity.rkt")

(provide door<%> door%)

(define door<%>
  (interface (entity<%>)
    get-destination
    set-place!
    is-lateral?
    place-destination))

(define door%
  (class* entity% (door<%>)
    (super-new)
    (init-field [dest #f] [place 'right])
    (inherit get-parent)

    ;; Accessor methods {{{
    ;
    (define/public get-destination
      (λ ()
        dest))
    ;; }}}
 
    ;; Mutator methods {{{
    ;
    (define/public set-place!
      (λ (np)
        ;; TODO add code that sets the x,y of door relative to room size
        (set! place np)))
    ;; }}}

    ;; Predicates {{{
    ;
    (define/public is-lateral?
      (λ ()
        (not (eq? place 'on-wall))))
    ;; }}}

    ;; Action methods {{{
    ;
    (define/public place-destination
      (λ ()
        (unless (send dest positioned?)
          (let ([parent (get-parent)])
            (unless (send parent positioned?)
              (error 'place-destination
                "called on a door whose parent has not been placed: ~a"
                parent))
            (let-values ([(x0 y0) (send parent get-pos)]
                         [(w0) (send parent get-width)])
              (send dest set-unbound-x! ((cond [(eq? place 'right) +]
                                               [(eq? place 'left) -])
                                         x0 w0))
              (send dest set-unbound-y! y0)))
          (send dest place-neighbors))))
    ;; }}}
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
