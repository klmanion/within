;;;; game-canvas.rkt

#lang racket/base

(require racket/class
  racket/gui/base
  racket/function)
(require "defs.rkt"
  "viewport/viewports.rkt"
  "langs/map/read-map.rkt")

(provide game-canvas%)

;;; game-canvas% {{{
;

(define game-canvas%
  (class canvas%
    (super-new [min-width 485]
               [min-height 300])
    (init-field [map-path #f])
    (field [player #f])
    (field [refresh-timer
             (new timer% [notify-callback (thunk (send this refresh))]
                         [interval 42])])
    (field [ship-camera (if (eq? map-path #f)
                            #f
                            (new ship-viewport%
                                 [subject (read-map map-path)]
                                 [aper-width 485] [aper-height 300]))])
    (inherit get-dc refresh)

    ;;; Initialization {{{

    ((thunk
       (when (ship-viewport? ship-camera)
         (set! player (send ship-camera get-parasite)))))
    ;; }}}

    ;;; Callbacks {{{

    (define/override on-paint
      (λ ()
        (let ([dc (get-dc)])
          (send dc set-background (make-object color% #x0 #x0 #x0))
          (send dc set-smoothing 'unsmoothed)
          (send dc clear)

          (unless (eq? ship-camera #f)
            (send ship-camera draw dc)))))

    (define/override on-size
      (λ (width height)
        (let ([dc (get-dc)]
              [scale (min (/ width 485)
                          (/ height 300))])
          (send dc set-scale scale scale))))

    (define/override on-char
      (λ (ch)
        (case (send ch get-key-code)
          [(left)  (send ship-camera left-callback)]
          [(right) (send ship-camera right-callback)]
          [(up)    (send ship-camera up-callback)]
          [(down)  (send ship-camera down-callback)])))

    (define/override on-event
      (λ (ev)
        (let-values ([(xscl yscl) (send (get-dc) get-scale)])
          (send ev set-x (floor (inexact->exact (/ (send ev get-x) xscl))))
          (send ev set-y (floor (inexact->exact (/ (send ev get-y) yscl)))))
        (cond
          [(and (not (eq? ship-camera #f))
                (send ship-camera in-viewport? ev))
           (send ship-camera on-event ev)])))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
