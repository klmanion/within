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
    get-x get-y get-pos
    get-width get-height
    set-x! set-y! set-pos!
    set-unbound-x! set-unbound-y! set-unbound-pos!
    positioned?
    draw))

(define entity%
  (class* child% (entity<%>)
    (super-new)
    (init-field [pos-x #f] [pos-y #f]
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
    ;; Positional variables {{{
    ;
    (define/public get-x
      (λ ()
        pos-x))

    (define/public get-y
      (λ ()
        pos-y))

    (define/public get-pos
      (λ ()
        (values (get-x) (get-y))))

    (define/public get-width
      (λ ()
        width))

    (define/public get-height
      (λ ()
        height))
    ;; }}}

    (define/private get-src-pos
      (λ ()
        (let ([src-x (* stage width)]
              [src-y (* form height)])
          (values src-x src-y))))
    ;; }}}

    ;; Mutator methods {{{
    ;
    ;; Positional variables {{{
    ;
    (define/public set-x!
      (λ (nx)
        (set! pos-x nx)))

    (define/public set-y!
      (λ (ny)
        (set! pos-y ny)))

    (define/public set-pos!
      (λ (nx ny)
        (set-x! nx)
        (set-y! ny)))

    (define/public set-unbound-x!
      (λ (nx)
        (when (eq? pos-x #f)
          (set-x! nx))))

    (define/public set-unbound-y!
      (λ (ny)
        (when (eq? pos-y #f)
          (set-y! ny))))

    (define/public set-unbound-pos!
      (λ (nx ny)
        (set-unbound-x! nx)
        (set-unbound-y! ny)))
    ;; }}}

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

    ;; Predicates {{{
    ;
    (define/public positioned?
      (λ ()
        (and (not (eq? pos-x #f))
             (not (eq? pos-y #f)))))
    ;; }}}
    
    ;; Action methods {{{
    ;
    (define/public draw
      (λ (dc)
        (unless (eq? bm #f)
          (let-values ([(src-x src-y) (get-src-pos)])
            (send dc draw-bitmap-section bm
                     pos-x pos-y src-x src-y
                     width height)))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
