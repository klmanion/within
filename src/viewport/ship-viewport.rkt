#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "viewport.rkt"
  "../ship.rkt"
  "../room.rkt"
  "../entity.rkt")

(provide ship-viewport<%> ship-viewport? ship-viewport%)

;; ship-viewport% {{{
;
(define ship-viewport<%>
  (interface (viewport<%>)
    get-parasite))

(define ship-viewport?
  (λ (o)
    (is-a? o ship-viewport<%>)))

(define ship-viewport%
  (class* viewport% (ship-viewport<%>)
    (super-new)
    (inherit get-offset-x get-offset-y
             get-aper-width get-aper-height)
    (inherit-field subject)

    (define/override draw
      (λ (dc)
        (for-each (λ (riv) ; room in view
                    (let ([xo (get-offset-x)]
                          [yo (get-offset-y)])
                      (send riv draw dc xo yo)))
                  (filter (λ (room)
                            (let-values ([(x y) (send room get-pos)])
                              (let* ([xo (get-offset-x)]
                                     [yo (get-offset-y)]
                                     [w (get-aper-width)]
                                     [h (get-aper-height)]
                                     [dx-m (- xo w)]
                                     [dx-M (+ xo w)]
                                     [dy-m (- yo h)]
                                     [dy-M (+ yo h)])
                                (and
                                  (not (or (eq? x #f)
                                           (eq? y #f)))
                                  (and (>= x dx-m)
                                       (<= x dx-M))
                                  (and (>= y dy-m)
                                       (<= y dy-M))))))
                          (send subject get-rooms)))))

    (define/public get-parasite
      (λ ()
        (if (eq? subject #f)
            #f
            (send subject get-parasite))))
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
