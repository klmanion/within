#lang racket/base
(require racket/class
  racket/gui/base)

(provide ship%)

;; ship% {{{
;
(define ship%
  (class object%
    (super-new)
    (init-field [rooms #f])
    
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
