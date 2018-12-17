;;;; viewport tracking-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)

(provide tracking<%> tracking? tracking/c)

(define tracking<%>
  (interface ((class->interface object%))
    get-start-x get-start-y get-start-pos
    get-end-x get-end-y get-end-pos
    get-coords
    is-tracking?
    start
    move
    finish
    clear))

(define tracking?
  (λ (o)
    (is-a? o tracking<%>)))

(define tracking/c
  (is-a?/c tracking<%>))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
