#lang racket/base
(require "testing.rkt")

(tests
 [(day0)
  0
  42
  24] 
 [(day1)
  (+ 1 1)
  (+ (+ 10 61) 1)
  (+ 1 (+ 3 3))
  (+ (+ 1 1) (+ 3 3))
  (- 1 1)
  (- (- 61 10) 1)
  (- 1 (- 3 3))
  (- (- 1 1) (- 3 3))
  (+ (- 61 10) 1)
  (- 10 (+ 3 3))
  (+ (- 1 1) (+ 3 3))
  (add1 1)
  (sub1 1)
  (add1 (+ 2 2))
  ((sub1 (- 6 2)) => 3)
  (! (bitwise-not #b00000000) => #"" #"" #b11111111)
  (! (bitwise-not #b11111111) => #"" #"" #b00000000)
  (bitwise-and 0 0)
  (bitwise-and 0 1)
  (bitwise-and 1 0)
  (bitwise-and 1 1)
  (bitwise-ior 0 0)
  (bitwise-ior 0 1)
  (bitwise-ior 1 0)
  (bitwise-ior 1 1)
  (bitwise-xor 0 0)
  (bitwise-xor 0 1)
  (bitwise-xor 1 0)
  (bitwise-xor 1 1)]
 [(day2)
  (* 1 1)
  (* 1 3)
  (* 4 3)
  (* (* 2 2) (* 4 1))
  (quotient 4 3)
  (remainder 4 3)
  (quotient 8 3)
  (remainder 8 10)
  (quotient 4 2)
  (quotient (quotient 4 1) (quotient 2 2))]
 [(day3)
  (= 2 2)
  (= 2 1)
  (= 2 3)
  (< 2 2)
  (< 2 1)
  (< 2 3)
  (<= 2 2)
  (<= 2 1)
  (<= 2 3)
  (> 2 2)
  (> 2 1)
  (> 2 3)
  (>= 2 2)
  (>= 2 1)
  (>= 2 3)
  (= (+ 2 2) (- 6 2))
  (= (* 2 2) (quotient 6 2))
  ((= (+ 2 2) (- 6 2)) => 1)
  ((< 4 (- 4 1)) => 0)]
 [(day4)
  (if0 0 3 4)
  (if0 1 3 4)
  ((if0 (- 1 1) 3 4) => 3)
  (if0 (+ 0 1) 3 4)
  ((if0 (= (+ 2 2) (- 6 2)) 3 4) => 4)
  ((if0 (< 4 (- 4 1)) 3 4) => 3)]
 [(day5)
  ((define (f a b)
     (+ a b))
   (app f 1 2))

  ((define (f a)
     a)
   (app f 1))
  
  (* 5 4)

  (* (+ 1 4) 4)

  ((define (g x y)
     (* x y))
   (app g 5 4))

  ((define (g x y)
     (* x y))
   (app g (+ 1 4) 4))

  ((define (f a b)
     (+ 1 a))
   (app f 4 4))

  ((define (f a b)
     b)
   (app f 4 4))

  ((define (f a b)
     (app g (+ 1 a) b))
   (define (g x y)
     (* x y))
   (app f 4 4))

  ((define (f a b)
     (app g (+ 1 a) b))
   (define (g x y)
     x)
   (app f 4 4))

  ((define (f a b)
     (app g (+ 1 a) b))
   (define (g x y)
     y)
   (app f 4 4))

  ((define (f a b)
     a)
   (app f 0 1))

  ((define (f a b)
     b)
   (app f 0 1))

  ((define (f a b)
     (app g a b))
   (define (g a b)
     a)
   (app f 0 1))

  ((define (f a b)
     (app g a b))
   (define (g a b)
     b)
   (app f 0 1))

  ((define (fac n)
     (app fac-help n 1))
   (define (fac-help n a)
     (if0 n
          a
          (app fac-help (- n 1) (* n a))))
   (app fac 5))]

 ;; adds first-class labels
 [(day6)
  ((define (choice f)
     (app f 1))
   (define (a x)
     (+ x 1))
   (define (b x)
     (+ x 2))
   (if0 (- 2 2)
        (app choice a)
        (app choice b)))]

 ;; adds vectors
 [(day7)
  ((define (set v)
     (seqn (vector-set! v 0 0)
           (vector-set! v 1 1)
           (vector-set! v 2 2)
           (app clean-up
                v
                (+ (vector-ref v 1)
                   (vector-ref v 2)))))
   (define (clean-up v ans)
     (seqn (free-vector! v)
           ans))
   (app set (make-vector 3)))])
