#lang racket/base
(require racket/class
  racket/gui/base)
(require "entity.rkt")

(provide parasite%)

;; parasite% {{{
;

(define parasite%
  (class entity%
    (super-new [pos-x 97] [pos-y 97]
               [width 5] [height 5]
               [color (make-object color% #xFF #xFF #xFF)])
    (inherit-field pos-x pos-y
                   width height
                   color)
    (inherit-field parent)

    ;; TODO get the correct y relative to the room's coordinates
    (define/public on-floor
      (λ ()
        void))  

    ;; Action methods {{{
    ;

    (define/override draw
      (λ (dc)
        (send dc set-pen color 0 'transparent)
        (send dc set-brush color 'solid)
        (send dc draw-rectangle pos-x pos-y
                                width height)))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
