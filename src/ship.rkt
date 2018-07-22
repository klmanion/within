#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "parent-child.rkt"
  "room.rkt")

(provide ship<%> ship? ship%)

;; ship% {{{
;
(define ship<%>
  (interface (parent<%>)
    get-rooms
    get-starting-room
    get-parasite))

(define ship?
  (λ (o)
    (is-a? o ship<%>)))

(define ship%
  (class* parent% (ship<%>)
    (super-new)
    (inherit get-children)

    ;; Superclass augmentation {{{
    ;
    (define valid-child?
      (λ (child)
        (is-a? child room<%>)))
    (augment valid-child?)
    ;; }}}

    ;; Accessor methods {{{
    ;
    (define/public get-rooms
      (λ ()
        (filter (λ (e)
                  (is-a? e room<%>))
                (get-children))))

    (define/public get-starting-room
      (λ ()
        (car (filter (λ (e)
                       (send e starting-room?))
                     (get-rooms)))))

    (define/public get-parasite
      (λ ()
        (let ([sr (get-starting-room)])
          (send sr get-parasite))))
    ;; }}}

    ;; Action methods {{{
    ;
    (define/public place-rooms
      (λ ()
        (let ([sr (get-starting-room)])
          (send sr set-pos! 0 0)
          (send sr place-neighbors))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
