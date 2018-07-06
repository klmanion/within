#lang racket/base
(require racket/class
  racket/gui/base)

;; room% {{{
;

(define room%
  (class object%
    (super-new)
    (init-field [name #f])

))

;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
