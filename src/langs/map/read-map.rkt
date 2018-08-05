#lang racket/base
(require racket/class
  racket/contract/base)
(require "../../ship.rkt")

(provide (contract-out
           [read-map (module-path? . -> . ship?)]))

(define read-map
  (λ (m)
    (let ([ship (dynamic-require m 'ship)])
      (send ship place-rooms)
      ship)))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
