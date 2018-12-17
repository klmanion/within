;;;; ship-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)
(require "parent-child.rkt")

(provide ship<%> ship? ship/c)

(define ship<%>
  (interface (parent<%>)
    get-rooms
    get-visible-rooms
    get-starting-room
    get-parasite
    place-rooms))

(define ship?
  (λ (o)
    (is-a? o ship<%>)))

(define ship/c
  (is-a?/c ship<%>))

; vim: set ts=2 sw=2 expandtab tw=79:
