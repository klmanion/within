#lang racket/base
(require racket/class
  racket/contract/base)
(require "../../ship.rkt")

(provide (contract-out
           [read-map (module-path? . -> . object?)]))

;; TODO dyn-r won't include the ship as a ship%
(define read-map
  (λ (m)
    (dynamic-require m 'ship)))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
