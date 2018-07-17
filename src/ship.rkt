#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)

(provide ship%)

;; ship% {{{
;
(define ship%
  (class object%
    (super-new)
    (init-field [rooms #f])

  (define/public add-child
    (λ (child)
      ;; TODO add a check for the child being a room% object
      (set! rooms (if (eq? rooms #f)
                      (list child)
                      (append rooms child)))))
    
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
