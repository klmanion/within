#lang racket/base
(require racket/class
  racket/contract
  racket/gui/base
  racket/function)
(require "../entity.rkt")

(provide viewport<%> viewport? viewport%)

;; viewport% {{{
;
(define viewport<%>
  (interface ((class->interface object%))
    get-x get-y get-pos
    get-aper-x get-aper-y get-aper-pos
    get-offset-x get-offset-y get-offsets
    get-aper-width get-aper-height get-aper-dimensions
    set-aper-width! set-aper-height! set-aper-dimensions!
    draw
    left-callback right-callback up-callback down-callback
    left/no-inv right/no-inv up/no-inv down/no-inv))

(define viewport?
  (λ (o)
    (is-a? o viewport<%>)))

(define/contract viewport%
  (class/c
    (init-field subject
                [pos-x real?] [pos-y real?]
                [aper-x real?] [aper-y real?]
                [aper-width real?] [aper-height real?]
                [inverted? boolean?])
    get-x get-y get-pos
    get-aper-x get-aper-y get-aper-pos
    get-aper-width get-aper-height get-aper-dimensions
    get-offset-x get-offset-y get-offsets
    [set-x! (real? . ->m . any)]
    [set-y! (real? . ->m . any)]
    [set-pos! (real? real? . ->m . any)]

    [set-aper-x! (real? . ->m . any)]
    [set-aper-y! (real? . ->m . any)]
    [set-aper-pos! (real? real? . ->m . any)]

    [set-aper-width! (real? . ->m . any)]
    [set-aper-height! (real? . ->m . any)]
    [set-aper-dimensions! (real? real? . ->m . any)]

    [draw ((is-a?/c dc<%>) . ->m . any)]

    [left-callback (->*m () (real?) any)]
    [right-callback (->*m () (real?) any)]
    [up-callback (->*m () (real?) any)]
    [down-callback (->*m () (real?) any)]

    [left/no-inv (->*m () (real?) any)]
    [right/no-inv (->*m () (real?) any)]
    [up/no-inv (->*m () (real?) any)]
    [down/no-inv (->*m () (real?) any)])
  (class* object% (viewport<%>)
    (super-new)
    (init-field [subject #f]
                [pos-x 0] [pos-y 0]
                [aper-x 0] [aper-y 0]
		            [aper-width 0] [aper-height 0]
                [inverted? #t])

    ;; Accessor methods {{{
    ;
    ;; Position methods {{{
    ;
    (define/public get-x
      (λ ()
        pos-x))

    (define/public get-y
      (λ ()
        pos-y))

    (define/public get-pos
      (λ ()
        (values (get-x) (get-y))))

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
        (- (get-aper-x) (get-x))))

    (define/public get-offset-y
      (λ ()
        (- (get-aper-y) (get-y))))

    (define/public get-offsets
      (λ ()
        (values (get-offset-x) (get-offset-y))))
    ;; }}}
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

    (define/public set-aper-x!
      (λ (nx)
        (set! aper-x nx)))

    (define/public set-aper-y!
      (λ (ny)
        (set! aper-y ny)))

    (define/public set-aper-pos!
      (λ (nx ny)
        (set-aper-x! nx)
        (set-aper-y! ny)))

    (define/public set-aper-width!
      (λ (nw)
        (set! aper-width nw)))

    (define/public set-aper-height!
      (λ (nh)
        (set! aper-height nh)))

    (define/public set-aper-dimensions!
      (λ (nw nh)
        (set-aper-width! nw)
        (set-aper-height! nh)))
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
