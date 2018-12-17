;;;; ship.rkt

#lang racket/base

(require racket/class
  racket/contract
  racket/gui/base
  racket/function)
(require "ship-inf.rkt"
  "parent-child.rkt"
  "room-inf.rkt")

(module+ test
  (require rackunit
    rackunit/text-ui))

(provide ship%
  (all-from-out "ship-inf.rkt"))

;;; Ship {{{
;

(define/contract ship%
  ;;; Contract {{{
  (class/c
    get-rooms
    get-visible-rooms
    get-starting-room
    get-parasite
    [place-rooms (->m any)])
  ;; }}}

  ;;; parent% {{{
  (class* parent% (ship<%>)
    (super-new)
    (inherit get-children)

    ;;; Augmentation {{{

    (define valid-child?
      (λ (child)
        (room? child)))
    (augment valid-child?)
    ;; }}}

    ;;; Accessors {{{

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

    ;;; Actions {{{

    (define/public place-rooms
      (λ ()
        (let ([sr (get-starting-room)])
          (send sr set-pos! 0 0)
          (send sr place-neighbors))))
    ;; }}}
  ) ; }}}
)
;;; }}}

;;; Unit tests {{{
;

(module+ test
  (require "parasite.rkt"
    "room.rkt")
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
