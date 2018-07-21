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
  (interface (parent<%> child<%>)
    get-name
    get-x get-y get-pos
    get-doors
    set-x! set-y! set-pos!
    set-unbound-x! set-unbound-y! set-unbound-pos!
    positioned?
    starting-room?
    name-equal?))

(define room%
  (class* parent-child% (room<%>)
    (super-new)
    (init-field [room-name #f])
    (init-field [pos-x #f] [pos-y #f])
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

    (define/public get-x
      (λ ()
        pos-x))

    (define/public get-y
      (λ ()
        pos-y))

    (define/public get-pos
      (λ ()
        (values (get-x) (get-y))))

    (define/public get-doors
      (λ ()
        (filter (λ (e)
                  (is-a? e door<%>))
                (get-children))))
    ;; }}}

    ;; Mutator methods {{{
    ;
    (define/public set-x!
      (λ (nx)
        (set! pos-x nx)))

    (define/public set-y!
      (λ (ny)
        (set! pos-y ny)))

    (define/public set-pos!
      (λ (nx ny)
        (set-x! nx)
        (set-y! ny)))

    (define/public set-unbound-x!
      (λ (nx)
        (when (eq? pos-x #f)
          (set-x! nx))))

    (define/public set-unbound-y!
      (λ (ny)
        (when (eq? pos-y #f)
          (set-y! ny))))

    (define/public set-unbound-pos!
      (λ (nx ny)
        (set-unbound-x! nx)
        (set-unbound-y! ny)))
    ;; }}}

    ;; Predicates {{{
    ;
    ;; General {{{
    ;
    (define/public positioned?
      (λ ()
        (and (not (eq? pos-x #f))
             (not (eq? pos-y #f)))))

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
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
