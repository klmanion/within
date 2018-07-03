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
         [(:: (:+ upper-case) whitespace)
          (token 'UCASE_WORD_TOK (string-trim lexeme))]
         [(:: (union alphabetic "'")
              (:* (complement whitespace)))
          (token 'WORD-TOK lexeme)]
         [(:+ numeric)
          (token 'INT-TOK (string->number lexeme))]
         [any-char (token 'CHAR_TOK lexeme #:skip? #t)])
       port))))
(provide make-tokenizer)

; vim: set ts=2 sw=2 expandtab lisp tw=79:
