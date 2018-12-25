;;;; entity.rkt

#lang racket/base

(require racket/class
  racket/contract
  racket/gui/base
  racket/function)
(require "entity-inf.rkt"
  "parent-child.rkt")

(provide entity%
  (all-from-out "entity-inf.rkt"))

;; entity% {{{
;

(define/contract entity%
  ;;; Contract {{{
  (class/c
    (init-field [pos-x (or/c false/c real?)]
                [pos-y (or/c false/c real?)]
                [width (or/c zero? (and/c real? positive?))]
                [height (or/c zero? (and/c real? positive?))])
    (init-field [bm (or/c false/c (is-a?/c bitmap%))]
                [form (or/c zero? (and/c real? positive?))]
                [stage (or/c zero? (and/c real? positive?))]
                [stage-limit (or/c false/c (and/c real? positive?))]
                [stride real?]
                [color (or/c false/c (is-a?/c color%))])
    (init-field [selectable? boolean?])
    (field [dest-x (or/c false/c real?)]
           [dest-y (or/c false/c real?)]
           [dest-theta (or/c false/c real?)])
    (field [move-timer (is-a?/c timer%)])
    get-x get-y get-pos
    get-dest-x get-dest-y get-dest-pos
    get-width get-height get-dimensions
    get-color
    [set-x! ((or/c false/c real?) . ->m . any)]
    [set-y! ((or/c false/c real?) . ->m . any)]
    [set-pos! (->m (or/c false/c real?) (or/c false/c real?) any)]

    [set-dest-x! ((or/c false/c real?) . ->m . any)]
    [set-dest-y! ((or/c false/c real?) . ->m . any)]
    [set-dest-pos! ((or/c false/c real?) (or/c false/c real?) . ->m . any)]


    [set-unbound-x! ((or/c false/c real?) . ->m . any)]
    [set-unbound-y! ((or/c false/c real?) . ->m . any)]
    [set-unbound-pos! ((or/c false/c real?) (or/c false/c real?) . ->m . any)]

    [set-width! (->m (or/c zero? (and/c real? positive?)) any)]
    [set-height! (->m (or/c zero? (and/c real? positive?)) any)]
    [set-dimensions! (->m (or/c zero? (and/c real? positive?))
                          (or/c zero? (and/c real? positive?))
                          any)]

    [positioned? (->m boolean?)] ; deprecated
    [is-positioned? (->m boolean?)]
    [is-selectable? (->m boolean?)]
    [is-moving-self? (->m boolean?)]

    [draw (->*m ((is-a?/c dc<%>)) (real? real?) any)]
    [start-move (real? real? . ->m . any)]
    [move (->m any)])
  ;; }}}

  ;;; entity% {{{
  (class* child% (entity<%>)
    (super-new)
    (init-field [pos-x #f] [pos-y #f]
                [width 0] [height 0])
    (init-field [bm #f]
                [form 0] [stage 0] [stage-limit #f]
                [stride 0]
                [color #f])
    (init-field [selectable? #f])
    (field [dest-x #f] [dest-y #f]
           [dest-theta #f] [new-dest? #f])
    (field [move-timer
            (new timer% [notify-callback (thunk
                                           (send this move))]
                        [interval 42])])

    ;;; Initialization {{{

    ((thunk
       (key-bitmap)))
    ;; }}}

    ;;; Accessors {{{

    ;;; Positional variables {{{
    (define/public get-x
      (λ ()
        pos-x))

    (define/public get-y
      (λ ()
        pos-y))

    (define/public get-pos
      (λ ()
        (values (get-x) (get-y))))

    (define/public get-dest-x
      (λ ()
        dest-x))

    (define/public get-dest-y
      (λ ()
        dest-y))

    (define/public get-dest-pos
      (λ ()
        (values (get-dest-x) (get-dest-y))))
    ;; }}}

    ;;; Dimensional variables {{{
    (define/public get-width
      (λ ()
        width))

    (define/public get-height
      (λ ()
        height))

    (define/public get-dimensions
      (λ ()
        (values (get-width) (get-height))))
    ;; }}}

    ;;; Sprite variables {{{
    (define/private get-bitmap
      (λ ()
        bm))

    (define/private get-form
      (λ ()
        form))

    (define/private get-stage
      (λ ()
        stage))

    (define/private get-stage-limit
      (λ ()
        stage-limit))

    (define/private get-src-pos
      (λ ()
        (let-values ([(w h) (get-dimensions)]
                     [(form) (get-form)]
                     [(stage) (get-stage)])
          (let ([src-x (* stage w)]
                [src-y (* form h)])
            (values src-x src-y)))))

    (define/public get-color
      (λ ()
        color))
    ;; }}}
    ;; }}}

    ;;; Mutators {{{

    ;;; Positional variables {{{
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

    (define/public move-x!
      (λ (dx)
        (set-x! (+ (get-x) dx))))

    (define/public move-y!
      (λ (dy)
        (set-y! (+ (get-y) dy))))

    (define/public move-pos!
      (λ (dx dy)
        (move-x! dx)
        (move-y! dy)))

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

    (define/private calc-dest-theta
      (λ ()
        (let-values ([(x y) (get-dest-pos)])
          (if (and (not (eq? x #f))
                   (not (eq? y #f)))
            (set! dest-theta (atan y x))
            (set! dest-theta #f)))))

    (define/public set-dest-x!
      (λ (nx)
        (set! dest-x nx)
        (set! new-dest? #t)
        (calc-dest-theta)))

    (define/public set-dest-y!
      (λ (ny)
        (set! dest-y ny)
        (set! new-dest? #t)
        (calc-dest-theta)))

    (define/public set-dest-pos!
      (λ (nx ny)
        (set! dest-x nx)
        (set! dest-y ny)
        (set! new-dest? #t)
        (calc-dest-theta)))
    ;; }}}

    ;;; Dimensional variables {{{
    (define/public set-width!
      (λ (nw)
        (set! width nw)))

    (define/public set-height!
      (λ (nh)
        (set! height nh)))

    (define/public set-dimensions!
      (λ (nw nh)
        (set-width! nw)
        (set-height! nh)))
    ;; }}}

    ;;; Sprite variables {{{
    (define/private set-form!
      (λ (nf)
        (when (>= nf 0)
          (set! form nf))))

    (define/private set-stage!
      (λ (ns)
        (when (>= ns 0)
          (set! stage ns))))

    (define/private set-stage-limit!
      (λ (nsl)
        (when (or (eq? nsl #f)
                  (>= nsl 0))
          (set! stage-limit nsl))))

    (define/private key-bitmap
      (λ ([color (get-color)])
        (let ([bm (get-bitmap)])
          (unless (or (eq? bm #f)
                      (eq? color #f))
            (let ([bm-dc (new bitmap-dc% [bitmap bm])]
                  [oclr (new color%)])
              (let-values ([(w h) (send bm-dc get-size)])
                (for* ([x (in-range w)]
                       [y (in-range h)])
                  (when (and (send bm-dc get-pixel x y oclr)
                             (not (= (send oclr alpha) 0)))
                    (send bm-dc set-pixel x y color)))))))))
    ;; }}}
    ;; }}}

    ;;; Predicates {{{

    (define/public positioned? ; deprecated
      (λ ()
        (and (not (eq? pos-x #f))
             (not (eq? pos-y #f)))))

    (define/public is-positioned?
      (λ ()
        (and (not (eq? pos-x #f))
             (not (eq? pos-y #f)))))

    (define/public is-selectable?
      (λ ()
        selectable?))

    (define/public is-moving-self?
      (λ ()
        (and (not (eq? (get-dest-x) #f))
             (not (eq? (get-dest-y) #f)))))
    ;; }}}
 
    ;;; Actions {{{

    (define/public draw
      (λ (dc [xo 0] [yo 0])
        (let ([bm (get-bitmap)])
          (unless (eq? bm #f)
            (let-values ([(src-x src-y) (get-src-pos)]
                         [(w h) (get-dimensions)])
              (let ([x (- (get-x) xo)]
                    [y (- (get-y) yo)])
                (send dc draw-bitmap-section bm
                         x y src-x src-y
                         w h)))))))

    (define/public start-move
      (λ (dest-x dest-y)
        (set-dest-pos! dest-x dest-y)))

    (define/public move
      (λ ()
        (when (is-moving-self?)
          (let-values ([(x0 y0) (get-pos)]
                       [(xf yf) (get-dest-pos)])
            (let ([dx (- xf x0)]
                  [dy (- yf y0)])
              (let ([theta (atan (/ dy dx))])
                (let ([x1 (* stride (cos theta))]
                      [y1 (* stride (sin theta))])
                  (set-pos! x1 y1))))))))
     ;; }}}
  ) ; }}}
)
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
