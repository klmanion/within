#lang racket/base
(require racket/class
  racket/gui/base)
(require "entity.rkt")

(provide parasite<%> parasite? parasite%)

;; parasite% {{{
;
(define parasite<%>
  (interface (entity<%>)
    on-floor))

(define parasite?
  (λ (o)
    (is-a? o parasite<%>)))

(define parasite%
  (class* entity% (parasite<%>)
    (super-new [pos-x 97] [pos-y 97]
               [width 5] [height 5]
               [color (make-object color% #xFF #xFF #xFF)])
    (inherit get-parent)
    (inherit get-color)
    (inherit get-x get-y get-width get-height
             set-x! set-y!)

    (define/public on-floor
      (λ ()
        (let ([rh (send (get-parent) get-height)])
          (set-y! (- rh (get-width))))))

    ;; Action methods {{{
    ;
    (define/override draw
      (λ (dc)
        (let ([color (get-color)])
          (send dc set-pen color 0 'transparent)
          (send dc set-brush color 'solid)
          (send dc draw-rectangle (get-x) (get-y)
                                  (get-width) (get-height)))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
