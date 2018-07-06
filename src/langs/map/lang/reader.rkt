#lang s-exp syntax/module-reader

map

#:read read
#:read-syntax read-syntax

(require syntax/strip-context)
(require "tokenizer.rkt"
  "parser.rkt")

(define read-syntax
  (λ (path port)
    (strip-context
      #`(module map-module map/lang/expander
          #,(parse path (make-tokenizer port path))))))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
