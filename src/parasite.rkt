;;;; parasite.rkt

#lang racket/base

(require racket/class
  racket/contract
  racket/gui/base)
(require "parasite-inf.rkt"
  "entity.rkt")

(provide parasite%
  (all-from-out "parasite-inf.rkt"))

;;; Parasite {{{
;

(define/contract parasite%
  ;;; Contract {{{
  (class/c
    [on-floor (->m any)]
    [draw (->*m ((is-a?/c dc<%>)) (real? real?) any)]
    [move (->m any)])
  ;; }}}

  ;;; parasite% {{{
  (class* entity% (parasite<%>)
    (super-new [pos-x 97] [pos-y 97]
               [width 5] [height 5]
               [stride 40]
               [color (make-object color% #xFF #xFF #xFF)]
               [selectable? #t])

    (inherit get-parent)
    (inherit get-color)
    (inherit get-x get-y get-pos
             get-width get-height get-dimensions
             set-x! set-y! set-pos!)
    (inherit get-dest-pos set-dest-pos!)
    (inherit-field dest-theta new-dest?)
    (inherit-field stride)

    ;;; Predicates {{{

    (define/public on-floor
      (λ ()
        (let ([rh (send (get-parent) get-height)])
          (set-y! (- rh (get-width))))))
    ;; }}}

    ;;; Actions {{{

    (define/override draw
      (λ (dc [xo 0] [yo 0])
        (let ([color (get-color)]
              [x (- (get-x) xo)]
              [y (- (get-y) yo)]
              [width (get-width)]
              [height (get-height)])
          (send dc set-pen color 0 'transparent)
          (send dc set-brush color 'solid)
          (send dc draw-rectangle x y width height))))

    ;(define trajectory (λ (x) (void)))
;    (define/override move
;      (λ ()
;        (if new-dest?
;            (begin
;              (set! new-dest? #f)
;              (when (not (eq? dest-theta #f))
;                (set! trajectory
;                      (λ (x)
;                        (let-values ([(xd yd) (get-dest-pos)]
;                                     [(x0 y0) (get-pos)])
;                          (let ([move-right? (> xd x0)])
;                            (let ([a -1]
;                                  [h (* (/ stride 2) (if move-right? 1 -1))]
;                                  [k (/ stride 2)])
;                              (+ (* a (expt (- (- x x0) h) 2)) k))))))))
;            (begin
;              (when (not (eq? dest-theta #f))
;                (let-values ([(x y) (get-pos)]
;                             [(w h) (get-dimensions)]
;                             [(xd yd) (get-dest-pos)]
;                             [(yf) (send (get-parent) get-floor)])
;                  (let ([move-right? (> xd x)])
;                    (let* ([fa (trajectory x)]
;                           [nx ((if move-right? + -) x (/ stride 30))]
;                           [fb (trajectory nx)]
;                           [ny (- y (- fb fa))])
;                      (if (<= ny yf)
;                          (begin
;                            (set-pos! nx ny))
;                          (begin
;                            (set-pos! nx (- yf h))
;                            (set! new-dest? #t)))))))))))
    ;; }}}
  ) ; }}}
)
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
