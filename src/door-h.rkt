#lang racket/base

(require racket/class
  racket/contract)
(require "entity-h.rkt")

(provide door<%> door? door/c
  place/c)

(define door<%>
  (interface (entity<%>)
    get-destination get-place
    set-destination! set-place!
    is-lateral?
    place-destination))

(define door?
  (λ (o)
    (is-a? o door<%>)))

(define door/c
  (is-a?/c door<%>))

(define place/c
  (or/c 'left 'right 'on-wall))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
