#lang racket/base
(require racket/contract
         racket/match
         (prefix-in x86: "asm.rkt"))
  
(struct e () #:prefab)
(struct num e (n) #:prefab)

(struct binop e (op lhs rhs) #:prefab)

(define parse
  (match-lambda
   [`(+ ,lhs ,rhs)
    (binop x86:add
           (parse lhs)
           (parse rhs))]
   [`(- ,lhs ,rhs)
    (binop x86:sub
           (parse lhs)
           (parse rhs))]
   [(? byte? b)
    (num b)]))

#|
 We rely on the invariant that inner expression always return their
 value to EAX.
|#
(define to-asm
  (match-lambda
   [(binop op lhs rhs)
    #|
     First, evaluate the LHS and save the result on the stack. Then
     evaluate the RHS and move the result to EBX. Then restore the LHS
     to EAX. And do the math, storing the result in EAX.
    |#
    (x86:seqn
     ;; LHS -> stack
     (to-asm lhs)
     (x86:push x86:eax)

     ;; RHS -> ebx
     (to-asm rhs)
     (x86:mov x86:ebx x86:eax)

     ;; (op LHS RHS) -> eax
     (x86:pop x86:eax)
     (op x86:eax x86:ebx))]
   [(num b)
    (x86:seqn
     (x86:mov x86:eax b))]))

(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]))
