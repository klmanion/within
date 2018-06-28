#lang racket/base
(require racket/class
  racket/gui/base)
(require "entity.rkt")

(provide parasite%)

(define parasite%
  (class entity%
    (super-new [pos-x 97] [pos-y 97]
               [width 5] [height 5])
    (init-field [color (make-object color% #xFF #xFF #xFF)])
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
