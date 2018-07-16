#lang racket/base

(provide read-map)

(define read-map
  (λ (nm)
    (dynamic-require nm 'data)))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
