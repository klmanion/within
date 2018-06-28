#lang racket/base
(require racket/class
  racket/gui/base)

(provide parasite%)

(define parasite%
  (class object%
    (super-new)
    (init-field [color (make-object color% #xFF #xFF #xFF)])
    (field [pos-x 97] [pos-y 97]
           [width 5])

    (define/public draw
      (λ (dc)
        (send dc set-brush color 'solid)
        (send dc set-pen color 0 'transparent)
        (send dc draw-rectangle pos-x pos-y width width)))
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
