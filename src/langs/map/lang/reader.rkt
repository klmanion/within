#lang racket/base

(require syntax/strip-context
  racket/contract/base)
(require "tokenizer.rkt"
  "parser.rkt")

(define lang-read
  (λ (port)
    (syntax->datum (lang-read-syntax #f port))))
(provide (contract-out
          [rename lang-read read
           (input-port? . -> . any/c)]))

(define lang-read-syntax
  (λ (path port)
    (strip-context
      #`(module map-module map/lang/expander
          #,(parse path (make-tokenizer port path))))))
(provide (contract-out
          [rename lang-read-syntax read-syntax
           (any/c input-port? . -> . syntax?)]))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
