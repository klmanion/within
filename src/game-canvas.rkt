#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "entity.rkt"
  "parasite.rkt")

(provide game-canvas%)

(define game-canvas%
  (class canvas%
    (super-new [min-width 485]
               [min-height 300])
    (inherit get-dc refresh)
    (field [player (new parasite%)]
           [visible-entity-lst '()])
    (field [refresh-timer
             (new timer% [notify-callback (thunk (send this refresh))]
                         [interval 42])])

    ;; temporary
    ((thunk
       (set! visible-entity-lst
             (list
               (new graphical-entity%
                    [bm (make-object bitmap% "graphics/scientist.png"
                                     'png/alpha)]
                    [width 20] [height 40]
                    [color (make-object color% #xFF #xFF #xFF)])))))

    (define/override on-paint
      (λ ()
        (let ([dc (get-dc)])
          (send dc set-background (make-object color% #x0 #x0 #x0))
          (send dc set-smoothing 'unsmoothed)
          (send dc clear)

          (unless (eq? player #f)
            (send player draw dc))
          (unless (null? visible-entity-lst)
            (map (λ (entity)
                   (send entity draw dc))
                 visible-entity-lst)))))

    (define/override on-size
      (λ (width height)
        (let ([dc (get-dc)]
              [scale (min (/ width 485)
                          (/ height 300))])
          (send dc set-scale scale scale))))
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
