;;;; force-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)
(require "entity-inf.rkt")

(provide force<%>)

(define force<%>
  (interface (class->interface object%)
    ds dx dy))

;; vim: set ts=2 sw=2 expandtab lisp tw=79:
