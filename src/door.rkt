#lang racket/base

(require racket/class
  racket/gui/base)
(require "entity.rkt"
  "room.rkt")

(provide door<%> door%)

(define door<%>
  (interface (entity<%>)
    ))

(define door%
  (class* entity% (door<%>)
    (super-new)
    (init-field [dest #f])

))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
