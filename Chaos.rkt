#lang racket
(provide (all-defined-out))

(define SIZE 512)
(define n 64)

(define (matrix-get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set matrix i j val)
  (vector-set! (vector-ref matrix i) j val))

(define (flatten-matrix matrix)
  (vector->list (apply vector-append (vector->list matrix))))

(define (get-ranges matrix [nr 64] [size 8] [step 8])
  (apply
   append
   (for/list ([i (in-range 0 (* nr step) step)])
     (for/list ([j (in-range 0 (* nr step) step)])
       (define sum 0)
       (define sum^2 0)
       (define block
         (for/vector ([a (in-range i (+ i size))])
           (for/vector ([b (in-range j (+ j size))])
             (define bi (matrix-get matrix a b))
             (set! sum (+ sum bi))
             (set! sum^2 (+ sum^2 (sqr bi)))
             bi)))
       (list block sum sum^2)))))

(define (make-isometry matrix iso [size 8])
  (for/vector ([i size])
    (for/vector ([j size])
      (cond
        [(= iso 0) (matrix-get matrix i j)]
        [(= iso 1) (matrix-get matrix i (- size 1 j))]
        [(= iso 2) (matrix-get matrix (- size 1 i) j)]
        [(= iso 3) (matrix-get matrix (- size 1 j) (- size 1 i))]
        [(= iso 4) (matrix-get matrix j i)]
        [(= iso 5) (matrix-get matrix (- size 1 j) i)]
        [(= iso 6) (matrix-get matrix (- size 1 i) (- size 1 j))]
        [(= iso 7) (matrix-get matrix j (- size 1 i))]))))

(define (get-domains matrix [nr 63] [size 16] [step 8])
  (apply
   append
   (for/list ([i (in-range 0 (* nr step) step)])
     (apply
      append
      (for/list ([j (in-range 0 (* nr step) step)])
        (define sum 0)
        (define sum^2 0)
        (define block
          (for/vector ([a (in-range i (+ i 8))])
            (for/vector ([b (in-range j (+ j 8))])
              (define bi
                (quotient (+ (matrix-get matrix a b)
                             (matrix-get matrix a (+ step b))
                             (matrix-get matrix (+ step a) b)
                             (matrix-get matrix (+ step a) (+ step b)))
                          4))
              (set! sum (+ sum bi))
              (set! sum^2 (+ sum^2 (sqr bi)))
              bi)))
        (for/list ([i 8])
          (list (make-isometry block i)
                sum sum^2)))))))

(define (search-range lrange domains)
  (define range (first lrange))
  (define sum-r (second lrange))
  (define sum-r^2 (third lrange))
  (let loop ([error (expt 2 30)] [index 0] [S 0] [O 0] [domains domains] [it 0])
    [cond
      [(empty? domains) (list index S O)]
      [else
       (define domain (caar domains))
       (define sum-d (cadar domains))
       (define sum-d^2 (caddar domains))
       (define sum-rd (apply + (map * (flatten-matrix range) (flatten-matrix domain))))
       (define denom-s (- (* n sum-d^2) (sqr sum-d)))
       (define s (if (= 0 denom-s) 0
                     (/ (- (* n sum-rd) (* sum-r sum-d)) denom-s)))
       (define o (/ (- sum-r (* s sum-d)) n))
       (define Error (+ sum-r^2
                        (* s (+ (* s sum-d^2) (- (* 2 sum-rd)) (* 2 o sum-d)))
                        (* o (- (* o n) (* 2 sum-r)))))
       (if (< Error error)
           (loop Error it s o (rest domains) (add1 it))
           (loop error index S O (rest domains) (add1 it)))]]))

(define (search-ranges ranges domains [p-gauge #f])
  (for/list ([i ranges])
    (when p-gauge (send p-gauge set-value (add1 (send p-gauge get-value))))
    (search-range i domains)))

(define (get-decoding-domains matrix [nr 63] [size 16] [step 8])
  (list->vector
   (apply
    append
    (for/list ([i (in-range 0 (* nr step) step)])
      (apply
       append
       (for/list ([j (in-range 0 (* nr step) step)])
         (define block
           (for/vector ([a (in-range i (+ i 8))])
             (for/vector ([b (in-range j (+ j 8))])
               (quotient (+ (matrix-get matrix a b)
                            (matrix-get matrix a (+ step b))
                            (matrix-get matrix (+ step a) b)
                            (matrix-get matrix (+ step a) (+ step b)))
                         4))))
         (for/list ([i 8]) (make-isometry block i))))))))

(define (decode founds new-domains)
  (for/vector ([i founds])
    (define domain (vector-ref new-domains (first i)))
    (define s (second i))
    (define o (third i))
    (for/vector ([i 8])
      (for/vector ([j 8])
        (+ o (* s (matrix-get domain i j)))))))

(define (blocks->image-matrix blocks)
  (define new-matrix (for/vector ([i SIZE]) (make-vector SIZE)))
  (for ([block blocks] [index (in-naturals)])
    (define row (quotient index 64))
    (define column (remainder index 64))
    (for ([i 8])
      (for ([j 8])
        (matrix-set new-matrix (+ (* 8 row) i) (+ (* 8 column) j) (exact-floor (matrix-get block i j))))))
  new-matrix)