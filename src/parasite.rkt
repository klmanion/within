#lang racket/base
(require racket/class
  racket/contract
  racket/gui/base)
(require "entity.rkt")

(provide parasite<%> parasite? parasite/c parasite%)

;; parasite% {{{
;
(define parasite<%>
  (interface (entity<%>)
    on-floor))

(define parasite?
  (λ (o)
    (is-a? o parasite<%>)))

(define parasite/c
  (is-a?/c parasite<%>))

(define/contract parasite%
  (class/c
    [on-floor (->m any)]
    [draw (->*m ((is-a?/c dc<%>)) (real? real?) any)]
    [move (->m any)])

  (class* entity% (parasite<%>)
    (super-new [pos-x 97] [pos-y 97]
               [width 5] [height 5]
               [step 3]
               [color (make-object color% #xFF #xFF #xFF)]
               [selectable? #t])
    (inherit get-parent)
    (inherit get-color)
    (inherit get-x get-y get-width get-height
             set-x! set-y!
             set-dest-pos!)

    (define/public on-floor
      (λ ()
        (let ([rh (send (get-parent) get-height)])
          (set-y! (- rh (get-width))))))

    ;; Action methods {{{
    ;
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

    (define/override move
      (λ ()
        (void)))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
