#lang racket/base
(require "testing.rkt")
    
(test-program
 'day0 '0 #rx#"^$" #rx#"^$" 0)
(test-program
 'day0 '42 #rx#"^$" #rx#"^$" 42)
(test-program
 'day0 '24 #rx#"^$" #rx#"^$" 24)

(test-program
 'day1 '(+ 1 1)
 #rx#"^$" #rx#"^$" 2)
(test-program
 'day1 '(+ (+ 10 61) 1)
 #rx#"^$" #rx#"^$" 72)
(test-program
 'day1 '(+ 1 (+ 3 3))
 #rx#"^$" #rx#"^$" 7)
(test-program
 'day1 '(+ (+ 1 1) (+ 3 3))
 #rx#"^$" #rx#"^$" 8)

(test-program
 'day1 '(- 1 1)
 #rx#"^$" #rx#"^$" 0)
(test-program
 'day1 '(- 4 2)
 #rx#"^$" #rx#"^$" 2)

(test-program
 'day1 '(add1 1)
 #rx#"^$" #rx#"^$" 2)
(test-program
 'day1 '(sub1 1)
 #rx#"^$" #rx#"^$" 0)

(test-program
 'day1 '(sub1 (+ 2 2))
 #rx#"^$" #rx#"^$" 3)
(test-program
 'day1 '(add1 (+ 2 2))
 #rx#"^$" #rx#"^$" 5)

(test-program
 'day1 '(bitwise-not #b00000000)
 #rx#"^$" #rx#"^$" #b11111111)
(test-program
 'day1 '(bitwise-not #b11111111)
 #rx#"^$" #rx#"^$" #b00000000)

(test-program
 'day1 '(bitwise-and 0 0)
 #rx#"^$" #rx#"^$" 0)
(test-program
 'day1 '(bitwise-and 1 0)
 #rx#"^$" #rx#"^$" 0)
(test-program
 'day1 '(bitwise-and 0 1)
 #rx#"^$" #rx#"^$" 0)
(test-program
 'day1 '(bitwise-and 1 1)
 #rx#"^$" #rx#"^$" 1)

(test-program
 'day1 '(bitwise-ior 0 0)
 #rx#"^$" #rx#"^$" 0)
(test-program
 'day1 '(bitwise-ior 1 0)
 #rx#"^$" #rx#"^$" 1)
(test-program
 'day1 '(bitwise-ior 0 1)
 #rx#"^$" #rx#"^$" 1)
(test-program
 'day1 '(bitwise-ior 1 1)
 #rx#"^$" #rx#"^$" 1)

(test-program
 'day1 '(bitwise-xor 0 0)
 #rx#"^$" #rx#"^$" 0)
(test-program
 'day1 '(bitwise-xor 1 0)
 #rx#"^$" #rx#"^$" 1)
(test-program
 'day1 '(bitwise-xor 0 1)
 #rx#"^$" #rx#"^$" 1)
(test-program
 'day1 '(bitwise-xor 1 1)
 #rx#"^$" #rx#"^$" 0)

