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
               [stride 5]
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

    ;; }}}
  ) ; }}}
)
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
