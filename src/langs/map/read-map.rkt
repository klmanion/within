#lang racket/base

(provide read-map)

(define read-map
  (λ (m)
    (dynamic-require m 'ship)))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
