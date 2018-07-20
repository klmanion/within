#lang racket/base

(require racket/class
  racket/gui/base)
(require "entity.rkt"
  "room.rkt")

(provide door<%> door%)

(define door<%>
  (interface (entity<%>)
    ))

(define door%
  (class* entity% (door<%>)
    (super-new)
    (init-field [dest #f] [pos #f])

    (define/public set-pos!
      (λ (npos)
        ;; TODO add code that sets the x,y of door relative to room size
        (set! pos npos)))
))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
