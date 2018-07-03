#lang racket/base
(require racket/class
  racket/gui/base)
(require "game-canvas.rkt")

;; Within
;

(define f (new frame%
               [label "Within"]
               [style '(fullscreen-button)]))

(define cv (new game-canvas% [parent f]
                             [map "maps/test.rkt"]))

(module+ main
  (send f show #t))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
