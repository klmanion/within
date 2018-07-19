#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "parent-child.rkt"
  "room.rkt")

(provide ship<%> ship%)

;; ship% {{{
;
(define ship<%>
  (interface (parent<%>)
    ))

(define ship%
  (class* parent% (ship<%>)
    (super-new)

  (define valid-child?
    (λ (child)
      (is-a? child room<%>)))
  (augment valid-child?)
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
