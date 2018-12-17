;;;; entity-inf.rkt

#lang racket/base

(require racket/class)
(require "parent-child.rkt")

(provide entity<%> entity? entity/c)

(define entity<%>
  (interface (child<%>)
    get-x get-y get-pos
    get-dest-x get-dest-y get-dest-pos
    get-width get-height get-dimensions
    get-color
    set-x! set-y! set-pos!
    set-unbound-x! set-unbound-y! set-unbound-pos!
    set-dest-x! set-dest-y! set-dest-pos!
    positioned?
    is-positioned?
    is-selectable?
    draw
    move))

(define entity?
  (λ (o)
    (is-a? o entity<%>)))

(define entity/c
  (is-a?/c entity<%>))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
