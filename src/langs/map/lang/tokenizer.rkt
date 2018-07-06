#lang racket/base
(require racket/string)
(require brag/support
  (prefix-in : br-parser-tools/lex-sre))
(require "lexer.rkt")
(module+ test
  (require rackunit))

(define make-tokenizer
  (λ (port [path #f])
    (port-count-lines! port)
    (lexer-file-path path)
    (lambda ()
      (map-lexer port))))
(provide make-tokenizer)

(module+ test
  (apply-tokenizer-maker make-tokenizer #<<EOB
HEAD:

ROOM foo:
  on_floor
  .x 10
EOB
  ) 
)

; vim: set ts=2 sw=2 expandtab lisp tw=79:
