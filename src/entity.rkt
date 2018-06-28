#lang racket/base
(require racket/class
  racket/gui/base)

(provide entity% graphical-entity%)

(define entity%
  (class object%
    (super-new)
    (init-field [pos-x 0] [pos-y 0]
                [width 0] [height 0])

    (define/public draw
      (λ (dc)
        void))
))

(define graphical-entity%
  (class entity%
    (super-new)
    (init-field [bm #f])
    (init-field [form 0] [stage 0])
    (inherit-field pos-x pos-y)

    (define/private src-pos
      (λ ()
        (let ([src-x (* stage width)]
              [src-y (* form height)])
          (values src-x src-y))))
    
    (define/override draw
      (λ (dc)
        (unless (eq? bm #f)
          (let-values ([(src-x src-y) (src-pos)])
            (send dc draw-bitmap-section bm
                     pos-x pos-y src-x src-y
                     width height)))))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
