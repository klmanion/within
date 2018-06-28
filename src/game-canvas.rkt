#lang racket/base
(require racket/class
  racket/gui/base)
(require "parasite.rkt")

(provide game-canvas%)

(define game-canvas%
  (class canvas%
    (super-new [min-width 485]
               [min-height 300])
    (inherit get-dc refresh)
    (field [player (new parasite%)])

    (define/override on-paint
      (λ ()
        (let ([dc (get-dc)])
          (send dc set-background (make-object color% #x0 #x0 #x0))
          (send dc set-smoothing 'unsmoothed)
          (send dc clear)

          (unless (eq? player #f)
            (send player draw dc)))))

    (define/override on-size
      (λ (width height)
        (let ([dc (get-dc)]
              [scale (min (/ width 485)
                          (/ height 300))])
          (send dc set-scale scale scale))))
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
