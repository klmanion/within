#lang racket/base
(require racket/class
  racket/gui/base)

(provide room%)

;; room% {{{
;
(define room%
  (class object%
    (super-new)
    (init-field [parent #f]
                [name #f])

))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
