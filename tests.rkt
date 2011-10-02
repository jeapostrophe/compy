#lang racket/base
(require "testing.rkt")
    
(test-program
 'day0 '0 #rx#"^$" #rx#"^$" 0)
(test-program
 'day0 '42 #rx#"^$" #rx#"^$" 42)
(test-program
 'day0 '24 #rx#"^$" #rx#"^$" 24)
