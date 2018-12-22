;;;; force.rkt

#lang racket/base

(require racket/class
  racket/contract)
(require "force-inf.rkt"
  "entity-inf.rkt")

(provide force%
  (all-from-out "force-inf.rkt"))

(define force%
  (class* object% (force<%>)
    (super-new)
    (init-field [quantity 0]
                [theta 0])

    (define/public ds
      (λ (entity)
        (let* ([m (send entity get-mass)]
               [a (/ quantity m)])
          (let ([v0 (send entity get-velocity)]
                [t (/ 1 60)])
            (+ (* .5 a (sqr t)) (* v0 t))))))

    (define/public dx
      (λ (entity)
        (* (ds entity) (cos theta))))

    (define/public dy
      (λ (entity)
        (* (ds entity) (sin theta))))
  )
)

;; vim: set ts=2 sw=2 expandtab lisp tw=79:
