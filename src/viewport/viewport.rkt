#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "../entity.rkt")

(provide viewport<%> viewport? viewport%)

;; viewport% {{{
;
(define viewport<%>
  (interface ((class->interface object%))
    get-pos-x get-pos-y get-pos
    get-aper-x get-aper-y get-aper-pos
    get-offset-x get-offset-y get-offsets
    get-aper-width get-aper-height get-aper-dimensions
    draw
    left-callback right-callback up-callback down-callback
    left/no-inv right/no-inv up/no-inv down/no-inv))

(define viewport?
  (λ (o)
    (is-a? o viewport<%>)))

(define viewport%
  (class* object% (viewport<%>)
    (super-new)
    (init-field [subject #f]
                [pos-x 0] [pos-y 0]
                [aper-x 0] [aper-y 0]
		            [aper-width 0] [aper-height 0]
                [inverted? #t])

    ;; Getter methods {{{
    ;
    ;; Position methods {{{
    ;
    (define/public get-pos-x
      (λ ()
        pos-x))

    (define/public get-pos-y
      (λ ()
        pos-y))

    (define/public get-pos
      (λ ()
        (values (get-pos-x) (get-pos-y))))

    (define/public get-aper-x
      (λ ()
        aper-x))

    (define/public get-aper-y
      (λ ()
        aper-y))

    (define/public get-aper-pos
      (λ ()
        (values (get-aper-x) (get-aper-y))))

    (define/public get-aper-width
      (λ ()
        aper-width))

    (define/public get-aper-height
      (λ ()
        aper-height))

    (define/public get-aper-dimensions
      (λ ()
        (values (get-aper-width) (get-aper-height))))
    ;; }}}

    ;; Positional offsets {{{
    ;
    (define/public get-offset-x
      (λ ()
        (- (get-aper-x) (get-pos-x))))

    (define/public get-offset-y
      (λ ()
        (- (get-aper-y) (get-pos-y))))

    (define/public get-offsets
      (λ ()
        (values (get-offset-x) (get-offset-y))))
    ;; }}}
    ;; }}}

    (define/public draw
      (λ (dc)
        (void)))

    ;; Panning methods {{{
    ;
    (define panning-d 10)

    ;; Private workhorse functions {{{
    ;
    (define/private pan-left
      (λ ([d panning-d])
        (set! aper-x (- aper-x d))))

    (define/private pan-right
      (λ ([d panning-d])
        (set! aper-x (+ aper-x d))))

    (define/private pan-up
      (λ ([d panning-d])
        (set! aper-y (+ aper-y d))))

    (define/private pan-down
      (λ ([d panning-d])
        (set! aper-y (- aper-y d))))
    ;; }}}

    ;; Interface member functions {{{
    ;
    (define/public left-callback
      (λ ([d panning-d])
        (if inverted?
            (pan-right d)
            (pan-left d))))

    (define/public right-callback
      (λ ([d panning-d])
        (if inverted?
            (pan-left d)
            (pan-right d))))

    (define/public up-callback
      (λ ([d panning-d])
        (if inverted?
            (pan-down d)
            (pan-up d))))

    (define/public down-callback
      (λ ([d panning-d])
        (if inverted?
            (pan-up d)
            (pan-down d))))

    (define/public left/no-inv
      (λ ([d panning-d])
        (pan-left d)))

    (define/public right/no-inv
      (λ ([d panning-d])
        (pan-right d)))

    (define/public up/no-inv
      (λ ([d panning-d])
        (pan-up d)))

    (define/public down/no-inv
      (λ ([d panning-d])
        (pan-down d)))
    ;; }}}
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
