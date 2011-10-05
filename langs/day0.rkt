#lang racket/base
(require racket/contract
         racket/match
         (prefix-in x86: "asm.rkt"))
  
(struct e () #:prefab)
(struct num e (n) #:prefab)

(define parse
  (match-lambda
   [(? byte? b)
    (num b)]))

(define to-asm
  (match-lambda
   [(num b)
    (x86:seqn
     (x86:mov x86:eax b))]))

(define interp
  (match-lambda
   [(num b)
    b]))


(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]
  [interp (-> e? any/c)]))
