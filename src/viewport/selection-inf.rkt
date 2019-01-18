;;;; viewport selection-inf.rkt

#lang racket/base

(require racket/class
  racket/contract)

(provide selection<%> selection? selection/c)

(define selection<%>
  (interface ((class->interface object%))
    get-selection
    set-selection!
    add-entity!
    append-list!
    empty?
    clear
    send-members))

(define selection?
  (λ (o)
    (is-a? o selection<%>)))

(define selection/c
  (is-a?/c selection<%>))

; vim: set ts=2 sw=2 expandtab tw=79:
