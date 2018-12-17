;;;; parasite-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)
(require "entity-inf.rkt")

(provide parasite<%> parasite? parasite/c)

(define parasite<%>
  (interface (entity<%>)
    on-floor))

(define parasite?
  (λ (o)
    (is-a? o parasite<%>)))

(define parasite/c
  (is-a?/c parasite<%>))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
