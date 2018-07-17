#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "parent-child.rkt"
  "entity.rkt")

(provide room<%> room%)

;; room% {{{
;
(define room<%>
  (interface (parent<%> child<%>)
    ))

(define room%
  (class* parent-child% (room<%>)
    (super-new)
    (init-field [name #f])

    (define valid-child?
      (λ (child)
        (is-a? child entity<%>)))
    (augment valid-child?)
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
