;;;; room-inf.rkt

#lang racket/base

(require racket/class)
(require "entity-inf.rkt"
  "parent-child.rkt")

(provide room<%> room? room/c)

(define room<%>
  (interface (entity<%> parent<%>)
    get-name
    get-floor
    get-doors get-lateral-doors
    get-destinations get-lateral-destinations
    get-parasite
    starting-room?
    name-equal?
    place-neighbors))

(define room?
  (λ (o)
    (is-a? o room<%>)))

(define room/c
  (is-a?/c room<%>))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
