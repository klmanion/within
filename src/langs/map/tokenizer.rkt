#lang racket/base
(require brag/support
  (prefix-in : br-parser-tools/sre))

(define make-tokenizer
  (λ (port)
    (lambda ()
      ((lexer
         [(from/to ";" "\n")
          (token 'COMMENT lexeme #:skip? #t)]
         [(:: (:+ upper-case) whitespace)
          (token 'UCASE_WORD_TOK (string-trim lexeme))]
         [(:: alphabetic (:* (complement whitespace)))
          (token 'WORD-TOK lexeme)]
         [(:+ numeric)
          (token 'INT-TOK (string->number lexeme))]
         [(eof) 'eof]
         [any-char (token 'CHAR_TOK lexeme)])
       port))))
(provide make-tokenizer)

; vim: set ts=2 sw=2 expandtab lisp tw=79:
