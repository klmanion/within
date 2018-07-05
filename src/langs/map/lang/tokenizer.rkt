#lang racket/base
(require racket/string)
(require brag/support
  (prefix-in : br-parser-tools/lex-sre))

(define make-tokenizer
  (λ (port)
    (lambda ()
      ((lexer-srcloc
         [(from/to ";" "\n")
          (token 'COMMENT lexeme #:skip? #t)]
         [(from/to "\"" "\"")
          (token 'STR-TOK (trim-ends "\"" lexeme "\""))]
         [(:: upper-case
              (:* (union upper-case "_" "-")))
          (token 'UCASE-WORD-TOK lexeme)]
         [(:: (union alphabetic "'")
              (:* (union alphabetic "_" "-")))
          (token 'WORD-TOK lexeme)]
         [(:+ numeric)
          (token 'INT-TOK (string->number lexeme))]
         [whitespace
          (token 'WHITESPACE-TOK lexeme #:skip? #t)]
         [any-char lexeme])
       port))))
(provide make-tokenizer)

(module+ test
  (require rackunit)
  (apply-tokenizer-maker make-tokenizer #<<EOB
HEAD:

ROOM foo:
  on_floor
  .x 10
EOB
  ) 
)

; vim: set ts=2 sw=2 expandtab lisp tw=79:
