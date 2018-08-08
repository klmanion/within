#lang racket/base

(require racket/class
  racket/gui/base
  racket/function
  racket/syntax)
(require "entity.rkt")

(provide door<%> door? door%)

;; door% {{{
;
(define door<%>
  (interface (entity<%>)
    get-destination
    get-place
    set-place!
    is-lateral?
    place-destination))

(define door?
  (λ (o)
    (is-a? o door<%>)))

(define door%
  (class* entity% (door<%>)
    (super-new [width 3] [height 30]
               [color (make-object color% #xFF #xFF #xFF)])
    (init-field [dest #f] [place 'right])
    (inherit get-parent)
    (inherit get-pos get-dimensions get-color)
    (inherit set-pos!)
    (inherit positioned?)

    ;; Initialization {{{
    ;
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
                           [(wd hd) (get-dimensions)])
                (let ([place (get-place)])
                  (let ([xd (cond [(eq? place 'right) (- (+ xr wr) wd)]
                                  [(eq? place 'left) xr])]
                        [yd (cond [(or (eq? place 'right)
                                       (eq? place 'left)) (- (+ yr hr) hd)])])
                    (set-pos! xd yd))))))))
    ;; }}}

    ;; Accessor methods {{{
    ;
    (define/public get-destination
      (λ ()
        dest))

    (define/public get-place
      (λ ()
        place))
    ;; }}}
 
    ;; Mutator methods {{{
    ;
    (define/public set-place!
      (λ (np)
        ;; TODO add code that sets the x,y of door relative to room size
        (set! place np)))
    ;; }}}

    ;; Predicates {{{
    ;
    (define/public is-lateral?
      (λ ()
        (not (eq? place 'on-wall))))
    ;; }}}

    ;; Action methods {{{
    ;
    (define/public place-destination
      (λ ()
        (let ([dest (get-destination)])
          (unless (send dest positioned?)
            (let ([parent (get-parent)])
              (unless (send parent positioned?)
                (error 'place-destination
                  "called on a door whose parent has not been placed: ~a"
                  parent))
              (generate-pos)
              (let-values ([(x0 y0) (send parent get-pos)]
                           [(w0) (send parent get-width)])
                (send dest set-unbound-x! (cond [(eq? place 'right) (+ x0 w0)]
                                                [(eq? place 'left) x0]))
                (send dest set-unbound-y! y0)))
            (send dest place-neighbors)))))

    (define/override draw
      (λ (dc [xo 0] [yo 0])
        (when (positioned?)
          (let-values ([(xr yr) (get-pos)]
                       [(w h) (get-dimensions)]
                       [(color) (get-color)])
            (let ([x (- xr xo)]
                  [y (- yr yo)])
              (send dc set-pen color 0 'transparent)
              (send dc set-brush color 'solid)
              (send dc draw-rectangle x y w h))))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
