#lang racket
(provide DCT IDCT)

(define (shift- vec)
  (vector-map (λ(x) (- x 128)) vec))

(define (shift+ vec)
  (vector-map (λ(x) (+ x 128)) vec))

(define (matrix-get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (list->matrix lst)
  (for/vector ([i 8])
    (for/vector ([j 8])
      (list-ref lst (+ j (* i 8))))))

(define (DCT-pixel block i j)
  (define Ci (if (= i 0) (/ 1 (sqrt 2)) 1))
  (define Cj (if (= j 0) (/ 1 (sqrt 2)) 1))
  (define sum 0)
  (for ([x 8])
    (for ([y 8])
      (set! sum (+ sum (* (matrix-get block x y)
                          (cos (/ (* (+ 1 (* 2 x)) i pi) 16))
                          (cos (/ (* (+ 1 (* 2 y)) j pi) 16)))))))
  (* sum 0.25 Ci Cj))

(define (DCT block)
  (define shifted-block (vector-map shift- block))
  (for/vector ([i 8])
    (for/vector ([j 8])
      (DCT-pixel shifted-block i j))))

(define (IDCT-pixel block x y)
  (define sum 0)
  (for ([i 8])
    (for ([j 8])
      (define Ci (if (= i 0) (/ 1 (sqrt 2)) 1))
      (define Cj (if (= j 0) (/ 1 (sqrt 2)) 1))
      (set! sum (+ sum (* Ci Cj (matrix-get block i j)
                          (cos (/ (* (+ 1 (* 2 x)) i pi) 16))
                          (cos (/ (* (+ 1 (* 2 y)) j pi) 16)))))))
  (* sum 0.25))

(define (IDCT block)
  (vector-map
   shift+
   (for/vector ([i 8])
     (for/vector ([j 8])
       (IDCT-pixel block i j)))))

(define 8x8 (vector (vector 124 125 122 120 122 119 117 118)
                    (vector 121 121 120 119 119 120 120 118)
                    (vector 126 124 123 122 121 121 120 120)
                    (vector 124 124 125 125 126 125 124 124)
                    (vector 127 127 128 129 130 128 127 125)
                    (vector 143 142 143 142 140 139 139 139)
                    (vector 150 148 152 152 152 152 150 151)
                    (vector 156 159 158 155 158 158 157 156)))
