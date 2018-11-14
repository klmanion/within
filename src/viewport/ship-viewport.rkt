#lang racket/base
(require racket/class
  racket/contract
  racket/gui/base
  racket/function
  racket/list)
(require "viewport.rkt"
  "tracking.rkt"
  "selection.rkt"
  "../ship.rkt"
  "../room.rkt"
  "../entity.rkt")

(provide ship-viewport<%> ship-viewport? ship-viewport/c ship-viewport%)

;; ship-viewport% {{{
;
(define ship-viewport<%>
  (interface (viewport<%>)
    get-selection get-mouse-tracking
    get-selection-outline-color get-selection-fill-color
    set-selection! set-mouse-tracking!
    set-selection-outline-color! set-selection-fill-color!
    get-parasite))

(define ship-viewport?
  (λ (o)
    (is-a? o ship-viewport<%>)))

(define ship-viewport/c
  (is-a?/c ship-viewport<%>))

(define/contract ship-viewport%
  (class/c
    (field [selection (or/c false/c selection/c)]
           [mtrack tracking/c])
    (field [soclr (is-a?/c color%)]
           [sfclr (is-a?/c color%)])
    get-selection get-mouse-tracking
    get-selection-outline-color get-selection-fill-color
    [set-selection! ((or/c false/c selection/c) . ->m . any)]
    [set-mouse-tracking! ((or/c false/c tracking/c) . ->m . any)]
    [set-selection-outline-color!
     (->*m (byte? byte? byte?) ((real-in 0 1)) any)]
    [set-selection-fill-color!
     (->*m (byte? byte? byte?) ((real-in 0 1)) any)])

  (class* viewport% (ship-viewport<%>)
    (super-new)
    (field [selection #f]
           [mtrack (new tracking%)])
    (field [soclr (make-object color% #xFF #xFF #xFF 1.0)]
           [sfclr (make-object color% #xFF #xFF #xFF 0.0)])
    (inherit get-offsets get-aper-dimensions)
    (inherit get-subject)

    ;; Accessor methods {{{
    ;
    (define/public get-selection
      (λ ()
        selection))

    (define/public get-mouse-tracking
      (λ ()
        mtrack))

    (define/public get-selection-outline-color
      (λ ()
        soclr))

    (define/public get-selection-fill-color
      (λ ()
        sfclr))

    (define/public get-parasite
      (λ ()
        (let ([subject (get-subject)])
          (if (eq? subject #f)
              #f
              (send subject get-parasite)))))
    ;; }}}

    ;; Mutator methods {{{
    ;
    (define/public set-selection!
      (λ (nsel)
        (set! selection nsel)))

    (define/public set-mouse-tracking!
      (λ (nmt)
        (set! mtrack nmt)))

    (define/public set-selection-outline-color!
      (λ (r g b [a 1.0])
        (set! soclr (make-object color% r g b a))))

    (define/public set-selection-fill-color!
      (λ (r g b [a 1.0])
        (set! sfclr (make-object color% r g b a))))
    ;; }}}

    ;; Action methods {{{
    ;
    (define/private make-selection
      (λ ()
        (let ([selection (get-selection)]
              [mtrack (get-mouse-tracking)])
          (set-selection!
            (new selection%
                 [lst (foldl
                        (λ (e acc)
                          (let-values ([(x0 y0 x1 y1) (send mtrack get-coords)]
                                       [(xe ye) (send e get-pos)]
                                       [(we he) (send e get-dimensions)])
                            (if (and (and (>= (+ xe we) x0) (<= xe x1))
                                     (and (>= (+ ye he) y0) (<= ye y1)))
                                (cons e acc)
                                acc)))
                        '()
                        (append-map
                          (λ (r)
                            (filter (λ (e)
                                      (send e is-selectable?))
                                    (send r get-entities)))
                          (send (get-subject) get-visible-rooms)))])))))

    (define/override on-event
      (λ (ev)
        (let ([x (send ev get-x)]
              [y (send ev get-y)])
          (cond
            [(send mtrack is-tracking?)
             (cond
               [(send ev dragging?) (send mtrack move x y)]
               [(send ev button-up? 'left)
                (begin (send mtrack finish x y) (make-selection))]
               [else
                (begin (send mtrack finish) (make-selection))])]
            [else
             (case (send ev get-event-type)
               [(left-down) (begin
                              (set-selection! #f)
                              (send mtrack start x y))]
               [(right-down) (when (not (eq? selection #f))
                               (send selection send-members
                                     'set-dest-pos! x y))])]))))

    (define/override draw
      (λ (dc)
        (for-each (λ (riv) ; room in view
                    (let-values ([(xo yo) (get-offsets)])
                      (send riv draw dc xo yo)))
                  (filter (λ (room)
                            (let-values ([(x y) (send room get-pos)]
                                         [(xo yo) (get-offsets)]
                                         [(w h) (get-aper-dimensions)])
                              (let ([dx-m (- xo w)]
                                    [dx-M (+ xo w)]
                                    [dy-m (- yo h)]
                                    [dy-M (+ yo h)])
                                (and
                                  (not (or (eq? x #f)
                                           (eq? y #f)))
                                  (and (>= x dx-m)
                                       (<= x dx-M))
                                  (and (>= y dy-m)
                                       (<= y dy-M))))))
                          (send (get-subject) get-visible-rooms)))
        (let ([mtrack (get-mouse-tracking)])
          (when (send mtrack is-tracking?)
            (let-values ([(x0 y0 x1 y1) (send mtrack get-coords)])
              (let ([dx (- x1 x0)]
                    [dy (- y1 y0)]
                    [soclr (get-selection-outline-color)]
                    [sfclr (get-selection-fill-color)])
                (send dc set-pen soclr 1 'solid)
                (send dc set-brush sfclr 'transparent)
                (send dc draw-rectangle x0 y0 dx dy)))))))
    ;; }}}
))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
