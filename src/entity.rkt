#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "parent-child.rkt")

(provide entity<%> entity%)

;; entity% {{{
;
(define entity<%>
  (interface (child<%>)
    draw))

(define entity%
  (class* child% (entity<%>)
    (super-new)
    (init-field [pos-x 0] [pos-y 0]
                [width 0] [height 0]
                [color #f])
    (init-field [bm #f]
                [form 0] [stage 0])

    ;; Initialization {{{
    ;

    ;; Key bitmap {{{
    ;
    ((thunk
       (key-bitmap)))
    ;; }}}

    ;; }}}

    ;; Accessor methods {{{
    ;

    (define/private src-pos
      (λ ()
        (let ([src-x (* stage width)]
              [src-y (* form height)])
          (values src-x src-y))))
    ;; }}}

    ;; Mutator methods {{{
    ;

    (define/private key-bitmap
      (λ ([color color])
        (unless (or (eq? bm #f)
                    (eq? color #f))
          (let ([bm-dc (new bitmap-dc% [bitmap bm])]
                [oclr (new color%)])
            (let-values ([(w h) (send bm-dc get-size)])
              (for* ([x (in-range w)]
                     [y (in-range h)])
                (when (and (send bm-dc get-pixel x y oclr)
                           (not (= (send oclr alpha) 0)))
                  (send bm-dc set-pixel x y color))))))))
    ;; }}}
    
    ;; Action methods {{{
    ;
    (define/public draw
      (λ (dc)
        (unless (eq? bm #f)
          (let-values ([(src-x src-y) (src-pos)])
            (send dc draw-bitmap-section bm
                     pos-x pos-y src-x src-y
                     width height)))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
