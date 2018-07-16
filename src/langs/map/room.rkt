#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)

(provide room%)

;; room% {{{
;
(define room%
  (class object%
    (super-new)
    (init-field [parent #f]
                [name #f])

    ((thunk
      (unless (eq? parent #f)
        (send parent add-child this))))
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
