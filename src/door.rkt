;;;; door.rkt

#lang racket/base

(require racket/class
  racket/contract
  racket/gui/base
  racket/function
  racket/syntax)
(require "door-inf.rkt"
  "room.rkt"
  "entity.rkt")

(provide door%
  (all-from-out "door-inf.rkt"))

;;; Door {{{
;

(define/contract door%
  ;;; Contract {{{
  (class/c
    (init-field [dest (or/c false/c room/c)]
                [place place/c])
    get-destination
    get-place
    [set-destination! ((or/c false/c room/c) . ->m . any)]
    [set-place! (place/c . ->m . any)]
    [is-lateral? (->m boolean?)]
    [place-destination (->m any)]
    [draw (->*m ((is-a?/c dc<%>)) (real? real?) any)])
  ;; }}}

  ;;; door% {{{
  (class* entity% (door<%>)
    (super-new [width 3] [height 30]
               [color (make-object color% #xFF #xFF #xFF)])
    (init-field [dest #f] [place 'right])
    (inherit get-parent)
    (inherit get-pos get-dimensions get-color)
    (inherit set-pos!)
    (inherit positioned?)

    ;;; Initialization {{{

    ((thunk
       (generate-pos)))

    ;; only call when the parent changes
    (define/private generate-pos
      (λ ()
        (let ([room (get-parent)])
          (if (or (eq? room #f)
                  (not (send room positioned?)))
              (set-pos! #f #f)
              (let-values ([(xr yr) (send room get-pos)]
                           [(wr hr) (send room get-dimensions)]
                           [(wd hd) (get-dimensions)]
                           [(place) (get-place)])
                (let ([xd (cond [(eq? place 'right) (- (+ xr wr) wd)]
                                [(eq? place 'left) xr])]
                      [yd (cond [(or (eq? place 'right)
                                     (eq? place 'left)) (- (+ yr hr) hd)])])
                  (set-pos! xd yd)))))))
    ;; }}}

    ;;; Accessors {{{

    (define/public get-destination
      (λ ()
        dest))

    (define/public get-place
      (λ ()
        place))
    ;; }}}
 
    ;;; Mutators {{{

    (define/public set-destination!
      (λ (ndest)
        (when (identifier? ndest)
          (set! dest ndest))))

    (define/public set-place!
      (λ (np)
        (set! place np)
        (generate-pos)))
    ;; }}}

    ;;; Predicates {{{

    (define/public is-lateral?
      (λ ()
        (let ([place (get-place)])
          (or (eq? place 'left)
              (eq? place 'right)))))
    ;; }}}

    ;;; Actions {{{

    (define/public place-destination
      (λ ()
        (generate-pos)
        (let ([dest (get-destination)])
          (unless (send dest positioned?)
            (let ([parent (get-parent)])
              (unless (send parent positioned?)
                (error 'place-destination
                  "called on a door whose parent has not been placed: ~a"
                  parent))
              (let-values ([(x0 y0) (send parent get-pos)]
                           [(w0) (send parent get-width)]
                           [(wd) (send dest get-width)])
                (send dest set-unbound-x! (cond [(eq? place 'right) (+ x0 w0)]
                                                [(eq? place 'left) (- x0 wd)]))
                (send dest set-unbound-y! y0)
                (send dest place-neighbors)))))))

    (define/override draw
      (λ (dc [xo 0] [yo 0])
        (when (positioned?)
          (let-values ([(xa ya) (get-pos)]
                       [(w h) (get-dimensions)]
                       [(color) (get-color)])
            (let ([x (- xa xo)]
                  [y (- ya yo)])
              (send dc set-pen color 0 'transparent)
              (send dc set-brush color 'solid)
              (send dc draw-rectangle x y w h))))))
    ;; }}}
  ) ; }}}
)
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
