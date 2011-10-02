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
