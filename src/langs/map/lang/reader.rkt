#lang racket/base
(require "tokenizer.rkt"
  "parser.rkt")

(define read-syntax
  (λ (path port)
    (datum->syntax #f `(module map-module map/lang/expander
                         ,(parse path (make-tokenizer port))))))
(provide read-syntax)

; vim: set ts=2 sw=2 expandtab lisp tw=79:
