#lang racket/base

(require racket/class
  racket/gui/base)
(require "entity.rkt"
  "room.rkt")

(provide door<%> door%)

(define door<%>
  (interface (entity<%>)
    get-destination
    set-pos!
    is-lateral?))

(define door%
  (class* entity% (door<%>)
    (super-new)
    (init-field [dest #f] [pos 'right])

    ;; Accessor methods {{{
    ;
    (define/public get-destination
      (λ ()
        dest))
    ;; }}}
 
    ;; Mutator methods {{{
    ;
    (define/public set-pos!
      (λ (npos)
        ;; TODO add code that sets the x,y of door relative to room size
        (set! pos npos)))
    ;; }}}

    ;; Predicates {{{
    ;
    (define/public is-lateral?
      (λ ()
        (not (eq? pos 'on-wall))))
    ;; }}}
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
