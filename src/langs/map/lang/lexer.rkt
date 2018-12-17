;;;; langs map lang lexer.rkt

#lang racket/base

(require racket/string)
(require brag/support
  (prefix-in : br-parser-tools/lex-sre))

(define map-lexer
  (lexer-srcloc
    [(from/to ";" "\n")
     (token 'COMMENT-TOK lexeme #:skip? #t)]
    [(from/to "/*" "*/")
     (token 'COMMENT-TOK lexeme #:skip? #t)]
    [(from/to "\"" "\"")
     (token 'STR-LIT-TOK (trim-ends "\"" lexeme "\""))]
    [(:: upper-case
         (:* (union upper-case "_" "-")))
     (token 'UCASE-WORD-TOK lexeme)]
    [(:: alphabetic
         (:* (union alphabetic numeric "_" "-")))
     (token 'WORD-TOK lexeme)]
    [(:+ numeric)
     (token 'INT-TOK (string->number lexeme))]
    [whitespace
     (token 'WHITESPACE-TOK lexeme #:skip? #t)]
    [any-char lexeme]))
(provide map-lexer)

; vim: set ts=2 sw=2 expandtab lisp tw=79:
