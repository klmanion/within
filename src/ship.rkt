#lang racket/base
(require racket/class
  racket/contract
  racket/gui/base
  racket/function)
(require "parent-child.rkt"
  "room.rkt")
(module+ test
  (require rackunit
    rackunit/text-ui))

(provide ship<%> ship? ship/c ship%)

;; ship% {{{
;
(define ship<%>
  (interface (parent<%>)
    get-rooms
    get-visible-rooms
    get-starting-room
    get-parasite
    place-rooms))

(define ship?
  (λ (o)
    (is-a? o ship<%>)))

(define ship/c
  (is-a?/c ship<%>))

(define/contract ship%
  (class/c
    get-rooms
    get-visible-rooms
    get-starting-room
    get-parasite
    [place-rooms (->m any)])

  (class* parent% (ship<%>)
    (super-new)
    (inherit get-children)

    ;; Superclass augmentation {{{
    ;
    (define valid-child?
      (λ (child)
        (room? child)))
    (augment valid-child?)
    ;; }}}

    ;; Accessor methods {{{
    ;
    (define/public get-rooms
      (λ ()
        (filter (λ (e)
                  (room? e))
                (get-children))))

    (define/public get-visible-rooms ; TODO
      (λ ()
        (get-rooms)))

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

;; Unit tests {{{
;
(module+ test
  (require "parasite.rkt")
  (void (run-tests
    (test-suite "ship class"
      (test-suite "returning parasite"
        (test-case "returning starting room"
          (define ship (new ship%))
          (define sr (new room% [parent ship] [room-name "sr"]))
          (check-eq? (send ship get-starting-room) sr))
        (test-case "returning parasite"
          (define ship (new ship%))
          (define sr (new room% [parent ship] [room-name "sr"]))
          (define parasite (new parasite% [parent sr]))
          (check-eq? (send ship get-parasite) parasite)))
      (test-suite "testing predicate"
        (test-pred "with new ship%"
          ship?
          (new ship%))
        (test-pred "with modified ship%"
          ship?
          (new ship% [children (list (new room%))])))))))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
