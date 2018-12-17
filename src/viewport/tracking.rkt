#lang racket/base
(require racket/class
  racket/contract
  racket/function)
(require "tracking-inf.rkt")

(provide tracking%
  (all-from-out "tracking-inf.rkt"))

;;; Tracking {{{
;

(define/contract tracking%
  ;;; Contract {{{
  (class/c
    (field [x0 (or/c false/c real?)]
           [y0 (or/c false/c real?)]
           [x1 (or/c false/c real?)]
           [y1 (or/c false/c real?)])
    (field [finished? boolean?])
    get-start-x get-start-y get-start-pos
    get-end-x get-end-y get-end-pos
    get-coords

    [set-start-x! ((or/c false/c real?) . ->m . any)]
    [set-start-y! ((or/c false/c real?) . ->m . any)]
    [set-start-pos! ((or/c false/c real?) (or/c false/c real?) . ->m . any)]
    [set-end-x! ((or/c false/c real?) . ->m . any)]
    [set-end-y! ((or/c false/c real?) . ->m . any)]
    [set-end-pos! ((or/c false/c real?) (or/c false/c real?) . ->m . any)]
    [set-coords! (->m (or/c false/c real?) (or/c false/c real?)
                      (or/c false/c real?) (or/c false/c real?)
                      any)]

    [is-tracking? (->m boolean?)]

    [start (real? real? . ->m . any)]
    [move (real? real? . ->m . any)]
    [finish (->*m () (real? real?) any)]
    [clear (->m any)])
  ;; }}}

  ;;; tracking% {{{
  (class* object% (tracking<%>)
    (super-new)
    (field [x0 #f] [y0 #f]
           [x1 #f] [y1 #f])
    (field [finished? #f])

    ;;; Accessors {{{

    (define/public get-start-x
      (λ ()
        x0))

    (define/public get-start-y
      (λ ()
        y0))

    (define/public get-start-pos
      (λ ()
        (values (get-start-x) (get-start-y))))

    (define/public get-end-x
      (λ ()
        x1))

    (define/public get-end-y
      (λ ()
        y1))

    (define/public get-end-pos
      (λ ()
        (values (get-end-x) (get-end-y))))

    (define/public get-coords
      (λ ()
        (let-values ([(x0 y0) (get-start-pos)]
                     [(x1 y1) (get-end-pos)])
          (values
            (min x0 x1)
            (min y0 y1)
            (max x0 x1)
            (max y0 y1)))))
    ;; }}}

    ;;; Mutators {{{

    (define/public set-start-x!
      (λ (nx)
        (set! x0 nx)))

    (define/public set-start-y!
      (λ (ny)
        (set! y0 ny)))

    (define/public set-start-pos!
      (λ (nx ny)
        (set-start-x! nx)
        (set-start-y! ny)))

    (define/public set-end-x!
      (λ (nx)
        (set! x1 nx)))

    (define/public set-end-y!
      (λ (ny)
        (set! y1 ny)))

    (define/public set-end-pos!
      (λ (nx ny)
        (set-end-x! nx)
        (set-end-y! ny)))

    (define/public set-coords!
      (λ (nx0 ny0 nx1 ny1)
        (set-start-pos! nx0 ny0)
        (set-end-pos! nx1 ny1)))
    ;; }}}

    ;;; Predicates {{{

    (define/public is-tracking?
      (λ ()
        (and (not finished?)
             (not (eq? x0 #f))
             (not (eq? y0 #f)))))
    ;; }}}

    ;;; Actions {{{

    (define/public start
      (λ (x y)
        (clear)
        (set-start-pos! x y)
        (set-end-pos! x y)))

    (define/public move
      (λ (x y)
        (set-end-pos! x y)))

    (define/public finish
      (λ ([x (get-end-x)] [y (get-end-y)])
        (move x y)
        (set! finished? #t)))

    (define/public clear
      (λ ()
        (set-coords! #f #f #f #f)
        (set! finished? #f)))
    ;; }}}
  ) ; }}}
)
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
