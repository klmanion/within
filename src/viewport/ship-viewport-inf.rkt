;;;; viewport ship-viewport-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)
(require "viewport-inf.rkt")

(provide ship-viewport<%> ship-viewport? ship-viewport/c)

(define ship-viewport<%>
  (interface (viewport<%>)
    get-selection get-mouse-tracking
    get-selection-outline-color get-selection-fill-color
    set-selection! set-mouse-tracking!
    set-selection-outline-color! set-selection-fill-color!
    get-parasite))

(define ship-viewport?
  (λ (o)
    (is-a? o ship-viewport<%>)))

(define ship-viewport/c
  (is-a?/c ship-viewport<%>))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
