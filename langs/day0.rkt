#lang racket/base
(require racket/contract
         racket/match
         "asm.rkt")
  
(struct e () #:prefab)
(struct num e (n) #:prefab)

(define parse
  (match-lambda
   [(? byte? b)
    (num b)]))

(define to-asm
  (match-lambda
   [(num b)
    (asm-begin
     (mov eax b))]))

(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? asm?)]))
