#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "defs.rkt"
  "langs/map/read-map.rkt")

(provide game-canvas%)

;; game-canvas% {{{
;

(define game-canvas%
  (class canvas%
    (super-new [min-width 485]
               [min-height 300])
    (inherit get-dc refresh)
    (init-field [ship #f])
    (field [player (new parasite%)]
           [visible-entity-lst '()])
    (field [refresh-timer
             (new timer% [notify-callback (thunk (send this refresh))]
                         [interval 42])])

    ;; Initialization {{{
    ;

    ((thunk
       (when (string? ship)
         (set! ship (read-map ship)))))

    ;; }}}

    ;; Callback methods {{{
    ;

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
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
