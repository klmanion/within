#lang racket/base
(require racket/class
  racket/gui/base)
(require "entity.rkt")

(provide parasite%)

(define parasite%
  (class entity%
    (super-new [pos-x 97] [pos-y 97])
    (init-field [color (make-object color% #xFF #xFF #xFF)])
    (field [width 5])
    (inherit-field pos-x pos-y)

    (define/public draw
      (λ (dc)
        (send dc set-brush color 'solid)
        (send dc set-pen color 0 'transparent)
        (send dc draw-rectangle pos-x pos-y width width)))
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
