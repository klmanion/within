;;;; viewport viewport-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)

(provide viewport<%> viewport? viewport/c)

(define viewport<%>
  (interface ((class->interface object%))
    get-x get-y get-pos
    get-aper-x get-aper-y get-aper-pos
    get-offset-x get-offset-y get-offsets
    get-aper-width get-aper-height get-aper-dimensions
    set-aper-width! set-aper-height! set-aper-dimensions!
    in-viewport?
    on-event
    draw
    left-callback right-callback up-callback down-callback
    left/no-inv right/no-inv up/no-inv down/no-inv))

(define viewport?
  (λ (o)
    (is-a? o viewport<%>)))

(define viewport/c
  (is-a?/c viewport<%>))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
