#lang racket/gui
(require "DCT.rkt" "Chaos.rkt")

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

;------------------------------------GUI----------------------------------------

(define frame
  (new frame%
       [label "Fractal Coding"]
       [x 300] [y 150]
       [width 1150] [height 675]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;------------------------------------ENCODE PANEL----------------------------------------

(define encode-panel
  (new vertical-panel%
       [parent main-panel]))

(define encode-bitmap (make-bitmap SIZE SIZE))
(define encode-dc (send encode-bitmap make-dc))
(send encode-dc set-background (make-color 0 0 0))
(send encode-dc clear)

(define encode-canvas
  (new canvas%
       [parent encode-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap encode-bitmap 20 20))]))

(define gauge-process
  (new gauge%
       [parent encode-panel]
       [label ""]
       [range 2048]
       [horiz-margin 100]))

(define ranges #f)
(define domains #f)
(define DCs #f)
(define image-name #f)
(define encode-buffer (make-bytes (* SIZE SIZE 4)))
(define original-matrix #f)

(define load-button-encode
  (new button%
       [parent encode-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../Chaos/utils" #f #f null))
          (when path
            (set! image-name (last (string-split (path->string path) "\\")))
            (set! encode-bitmap (read-bitmap path))
            (send encode-canvas on-paint)
            (send gauge-process set-value 0)
            (send encode-bitmap get-argb-pixels 0 0 SIZE SIZE encode-buffer)
            (set! original-matrix (get-matrix encode-buffer))
            (define blocks (get-blocks original-matrix))
            (define dct-blocks (map DCT blocks))
            (set! DCs (get-coefficients 0 0 dct-blocks))
            (define DC-blocks (coefs->matrix DCs))
            (set! ranges (get-ranges DC-blocks))
            (set! domains (get-domains DC-blocks))))]))

(define founds #f)
(define process-button
  (new button%
       [parent encode-panel]
       [label "Process"]
       [callback
        (λ (button event)
          (when (and ranges domains)
            (set! founds
                  (time
                   (search-ranges ranges domains)))))]))

;------------------------------------DECODE PANEL----------------------------------------

(define decode-panel
  (new vertical-panel%
       [parent main-panel]))

(define decode-bitmap (make-bitmap SIZE SIZE))
(define decode-dc (send decode-bitmap make-dc))
(send decode-dc set-background (make-color 0 0 0))
(send decode-dc clear)

(define decode-canvas
  (new canvas%
       [parent decode-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap decode-bitmap 20 20))]))

(define revDcs #f)
(define fractal-matrix (for/vector ([i SIZE]) (make-vector SIZE)))
(define decode-button
  (new button%
       [parent decode-panel]
       [label "Decode"]
       [callback
        (λ (button event)
          (when founds
            (define blocks (decode founds (get-decoding-domains fractal-matrix)))
            (set! fractal-matrix (small-blocks->matrix blocks))
            (set! revDcs (map exact-round (flatten-matrix fractal-matrix)))
            (printf "~a\n" (take revDcs 10))))]))

(define (normalize x)
  (set! x (exact-round x))
  (cond [(< x 0) 0] [(> x 255) 255] [else x]))

(define (matrix->bytes matrix)
  (list->bytes (apply append (map (λ(x) (list 255 x x x)) (flatten-matrix matrix)))))

#|(define idcted-matrix #f)
(define dcted-blocks #f)
(define finalize-button
  (new button%
       [parent decode-panel]
       [label "Finalize"]
       [callback
        (λ (button event)
          (set! dcted-blocks (map first (get-ranges fractal-matrix)))
          (set! idcted-matrix (small-blocks->matrix dcted-blocks))
          (set! idcted-matrix
                (for/vector ([i SIZE])
                  (for/vector ([j SIZE])
                    (normalize (matrix-get idcted-matrix i j)))))
          (send decode-bitmap set-argb-pixels 0 0 SIZE SIZE (matrix->bytes idcted-matrix))
          (send decode-canvas on-paint))]))|#