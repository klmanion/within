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
      (test-equal? "parsing of test program"
        (syntax->datum
          (parse #f (apply-tokenizer-maker make-tokenizer #<<EOB
HEAD {
}

ROOM foo {
  PARASITE {
    on-floor
    .x 10
  }
}
EOB
          )))
        '(program
           (clause
            (clause-head (clause-name "HEAD"))
            (clause-body))
           (clause
            (clause-head (clause-name "ROOM") "foo")
            (clause-body
              (clause
                (clause-head (clause-name "PARASITE"))
                (clause-body
                  (directive "on-floor")
                  (assignment (member-id "x")
                              (rvalue 10))))))))))))
  
; vim: set ts=2 sw=2 expandtab lisp tw=79:
