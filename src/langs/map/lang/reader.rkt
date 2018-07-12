#lang racket/base

(require syntax/strip-context)
(require "tokenizer.rkt"
  "parser.rkt")

(define lang-read-syntax
  (λ (path port)
    (strip-context
      #`(module map-module map/lang/expander
          #,(parse path (make-tokenizer port path))))))
(provide (rename-out [lang-read-syntax read-syntax]))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
