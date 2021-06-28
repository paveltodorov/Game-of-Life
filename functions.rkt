#lang racket
(require "2.rkt")
(define (displaymatrix a)
(cond
  [(empty?  a) (display "\n")]
  [else(display (car a))
       (display "\n")
       (displaymatrix (cdr a))]))

(define(find-element i j matrix)
(cond
  [ (< i 0) (find-element (- h 1) j matrix) ]
  [ (< j 0) (find-element i (- w 1) matrix) ]
  [ (> i (- h 1)) (find-element 0 j matrix) ]
  [ (> j (- w 1)) (find-element i 0 matrix) ]
  [else (list-ref (list-ref matrix i) j )] ))

(define (a i j matrix)
 (if (equal? (find-element i j matrix)".") 0 1))

(define (isdead i j matrix) (equal? (list-ref (list-ref matrix i) i ) "."  ))

(define (isliving i j matrix) (equal? (list-ref (list-ref matrix i) j ) "■"  ))

(define (number-of-n i j matrix)
(+ (a (- i 1) j matrix) (a i (- j 1) matrix) (a (- i 1) (- j 1) matrix)
  (a (+ i 1) j matrix) (a i (+ 1 j) matrix) (a (+ 1 i) (+ 1 j) matrix)
  (a (+ i 1) (- j 1) matrix) (a (- i 1) (+ j 1) matrix)))

(define (help2 i j matrix)
(cond
  [(= j w) '()]
  [(and (isliving i j matrix) (or (= (number-of-n i j matrix) 2)(= (number-of-n i j matrix) 3))) (cons "■" (help2 i (+ 1 j) matrix))  ]
  [(and (isdead i j matrix) (= (number-of-n i j matrix) 2)) (cons "■" (help2 i (+ 1 j) matrix)) ]
  [else (cons "." (help2 i (+ 1 j) matrix))]))


(define (next-gen matrix)
  (define (help1 i j matrix)
(cond
  [(= i h) '() ]
  [else (cons (help2 i j matrix) (help1 (+ i 1) j matrix))]))
   (help1 0 0 matrix))

(define (f generation matrix)
  (define (help3 generation curr-gen matrix)
  (cond
    [(=  generation curr-gen) (display "\n") ]
    [else(displaymatrix (next-gen matrix))
         (help3 generation (+ 1 curr-gen) (next-gen matrix))]))
(help3 generation 0 matrix))
