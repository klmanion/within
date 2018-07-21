#lang racket/base

(require brag/support
  syntax/strip-context)
(require "parser.rkt"
  "tokenizer.rkt")

(module+ test
  (require rackunit
    rackunit/text-ui)
  (void (run-tests
    (test-suite "parser"
      (check-equal?
        (syntax->datum
          (parse #f (apply-tokenizer-maker make-tokenizer #<<EOB
HEAD:

ROOM foo:
  on_floor
  .x 10
EOB
          )))
        '(program
           (clause
            (clause-head (clause-name "HEAD"))
            (clause-body))
           (clause
            (clause-head (clause-name "ROOM") "foo")
            (clause-body
              (directive "on_floor")
              (assignment (member-id "x")
                          (rvalue 10)))))
        "failed to parse test program")))))
  
; vim: set ts=2 sw=2 expandtab lisp tw=79: