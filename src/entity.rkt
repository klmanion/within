#lang racket/base
(require racket/class
  racket/gui/base)

(provide entity%)

(define entity%
  (class object%
    (super-new)
    (init-field [pos-x 0] [pos-y 0])
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
