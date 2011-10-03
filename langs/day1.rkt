#lang racket/base
(require racket/contract
         racket/match
         (prefix-in x86: "asm.rkt"))
  
(struct e () #:prefab)
(struct num e (n) #:prefab)

(struct binop e (op assoc? lhs rhs) #:prefab)
(struct unaop e (op lhs) #:prefab)

(define binops
  (hasheq '+ (cons x86:add #t)
          '- (cons x86:sub #f)          
          'bitwise-and (cons x86:and #t)
          'bitwise-ior (cons x86:or #t)
          'bitwise-xor (cons x86:xor #t)))
(define unaops
  (hasheq 'add1 x86:inc
          'sub1 x86:dec
          'bitwise-not x86:not))

;; XXX Add *, quotient, remainder
(define parse
  (match-lambda
   [(list (and (? symbol?)
               (app (lambda (s)
                      (hash-ref binops s #f))
                    (cons opcode assoc?)))
          lhs
          rhs)
    (binop opcode assoc?
           (parse lhs)
           (parse rhs))]
   [(list (and (? symbol?)
               (app (lambda (s)
                      (hash-ref unaops s #f))
                    (and (not #f)
                         opcode)))
          lhs)
    (unaop opcode
           (parse lhs))]
   [(? byte? b)
    (num b)]))

#|
 We rely on the invariant that inner expression always return their
 value to EAX.
|#
(define to-asm
  (match-lambda
   [(binop op assoc? lhs rhs)
    #|
     First, evaluate the LHS and save the result on the stack. Then
     evaluate the RHS and move the result to EBX. Then restore the LHS
     to EAX. Then do the math, storing the result in EAX.

     If the operation is associative, we can get rid of one mov
    |#
    (x86:seqn
     ;; LHS -> stack
     (to-asm lhs)
     (x86:push x86:eax)

     ;; RHS -> eax
     (to-asm rhs)

     (if assoc?
         (x86:seqn
          ;; LHS -> ebx
          (x86:pop x86:ebx))
         (x86:seqn
          ;; RHS -> ebx
          (x86:mov x86:ebx x86:eax)
          ;; LHS -> eax
          (x86:pop x86:eax)))
     
     ;; (op LHS RHS) -> eax
     (op x86:eax x86:ebx))]
   [(unaop op lhs)
    (x86:seqn
     ;; LHS -> eax
     (to-asm lhs)
     ;; (op LHS) -> eax
     (op x86:eax))]
   [(num b)
    (x86:seqn
     (x86:mov x86:eax b))]))

(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]))
