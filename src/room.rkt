#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "parent-child.rkt"
  "entity.rkt"
  "door.rkt")

(provide room<%> room%)

;; room% {{{
;
(define room<%>
  (interface (entity<%> parent<%>)
    get-name
    get-doors get-lateral-doors
    get-destinations get-lateral-destinations
    starting-room?
    name-equal?
    place-neighbors))

(define room%
  (class* (parent-mixin entity%) (room<%>)
    (super-new)
    (init-field [room-name #f])
    (inherit get-children)

    ;; Superclass augmentation {{{
    ;
    (define valid-child?
      (λ (child)
        (is-a? child entity<%>)))
    (augment valid-child?)
    ;; }}}

    ;; Accessor methods {{{
    ;
    (define/public get-name
      (λ ()
        (if (eq? room-name #f)
            ""
            room-name)))

        (define/public get-doors
      (λ ()
        (filter (λ (e)
                  (is-a? e door<%>))
                (get-children))))

    (define/public get-lateral-doors
      (λ ()
        (filter (λ (door)
                  (send door is-lateral?))
                (get-doors))))

    (define/public get-destinations
      (λ ()
        (map (λ (door)
               (send door get-destination))
             (get-doors))))

    (define/public get-lateral-destinations
      (λ ()
        (map (λ (door)
               (send door get-destination))
             (get-lateral-doors))))
    ;; }}}

    ;; Mutator methods {{{
    ;
    ;; }}}

    ;; Predicates {{{
    ;
    ;; General {{{
    ;
    (define/public starting-room?
      (λ ()
        (name-equal? "sr" "starting_room" "starting-room")))
    ;; }}}

    ;; Variable specific {{{
    ;
    (define/public name-equal?
      (λ (str . bss)
        (or (string=? (get-name) str)
            (and (not (null? bss))
                 (name-equal? (car bss) (cdr bss))))))
    ;; }}}
    ;; }}}

    ;; Action Methods {{{
    ;
    (define/public place-neighbors
      (λ ()
        (for-each
          (λ (door)
            (send door place-destination))
          (get-lateral-destinations))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
