#lang racket/base
(require racket/class
  racket/gui/base
  racket/function)
(require "parent-child.rkt"
  "entity.rkt"
  "door.rkt"
  "parasite.rkt")

(provide room<%> room? room%)

;; room% {{{
;
(define room<%>
  (interface (entity<%> parent<%>)
    get-name
    get-doors get-lateral-doors
    get-destinations get-lateral-destinations
    get-parasite
    starting-room?
    name-equal?
    place-neighbors))

(define room?
  (λ (o)
    (is-a? o room<%>)))

(define room%
  (class* (parent-mixin entity%) (room<%>)
    (super-new [width 200] [height 80]
               [color (make-object color% #xFF #xFF #xFF)])
    (init-field [room-name #f])
    (inherit get-children)
    (inherit get-pos get-dimensions
             get-color)
    (inherit positioned?)

    ;; Superclass augmentation {{{
    ;
    (define valid-child?
      (λ (child)
        (entity? child)))
    (augment valid-child?)
    ;; }}}

    ;; Accessor methods {{{
    ;
    (define/public get-name
      (λ ()
        (if (eq? room-name #f)
            ""
            room-name)))

    (define/public get-doors
      (λ ()
        (filter (λ (e)
                  (is-a? e door<%>))
                (get-children))))

    (define/public get-lateral-doors
      (λ ()
        (filter (λ (door)
                  (send door is-lateral?))
                (get-doors))))

    (define/public get-destinations
      (λ ()
        (map (λ (door)
               (send door get-destination))
             (get-doors))))

    (define/public get-lateral-destinations
      (λ ()
        (map (λ (door)
               (send door get-destination))
             (get-lateral-doors))))

    (define/public get-parasite
      (λ ()
        (let ([psite-lst (filter parasite? (get-children))])
          (if (null? psite-lst)
              psite-lst
              (car psite-lst)))))
    ;; }}}

    ;; Mutator methods {{{
    ;
    ;; }}}

    ;; Predicates {{{
    ;
    ;; General {{{
    ;
    (define/public starting-room?
      (λ ()
        (name-equal? "sr" "starting_room" "starting-room")))
    ;; }}}

    ;; Variable specific {{{
    ;
    (define/public name-equal?
      (λ (str . bss)
        (or (string=? (get-name) str)
            (and (not (null? bss))
                 (name-equal? . bss)))))
    ;; }}}
    ;; }}}

    ;; Action methods {{{
    ;
    (define/public place-neighbors
      (λ ()
        (for-each (λ (door)
                    (send door place-destination))
                  (get-lateral-doors))))

    (define/override draw
      (λ (dc [xo 0] [yo 0])
        (let ([children (get-children)])
          (unless (null? children)
            (for-each (λ (e)
                        (send e draw dc xo yo))
                      children)))
        (when (positioned?)
          (let-values ([(xa ya) (get-pos)]
                       [(w h) (get-dimensions)]
                       [(color) (get-color)])
            (let ([x (- xa xo)]
                  [y (- ya yo)])
              (send dc set-pen color 3 'solid)
              (send dc set-brush color 'transparent)
              (send dc draw-rectangle x y w h))))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
