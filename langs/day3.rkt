#lang racket/base
(require racket/contract
         racket/match
         (prefix-in x86: "asm.rkt"))
  
(struct e () #:prefab)
(struct num e (n) #:prefab)

(struct mult e (lhs rhs) #:prefab)
(struct div e (remainder? lhs rhs) #:prefab)
(struct cmpop e (op lhs rhs) #:prefab)
(struct binop e (op assoc? lhs rhs) #:prefab)
(struct unaop e (op lhs) #:prefab)

(define binops
  (hasheq '+ (cons x86:add #t)
          '- (cons x86:sub #f)          
          'bitwise-and (cons x86:and #t)
          'bitwise-ior (cons x86:or #t)
          'bitwise-xor (cons x86:xor #t)))
(define cmpops
  (hasheq '= x86:sete
          '> x86:setg
          '>= x86:setge
          '< x86:setl
          '<= x86:setle))
(define unaops
  (hasheq 'add1 x86:inc
          'sub1 x86:dec
          'bitwise-not x86:not))

(define parse
  (match-lambda
   [(list '* lhs rhs)
    (mult (parse lhs) (parse rhs))]
   [(list 'quotient lhs rhs)
    (div #f (parse lhs) (parse rhs))]
   [(list 'remainder lhs rhs)
    (div #t (parse lhs) (parse rhs))]
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
                      (hash-ref cmpops s #f))
                    (and (not #f)
                         opcode)))
          lhs rhs)
    (cmpop opcode
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
   [(mult lhs rhs)
    (x86:seqn
     ;; LHS -> eax
     (to-asm lhs)
     ;; LHS -> edx
     (x86:mov x86:edx x86:eax)
     ;; RHS -> eax
     (to-asm rhs)
     ;; (op RHS LHS) -> eax
     (x86:imul x86:edx))]
   [(div remainder? lhs rhs)
    (x86:seqn
     (to-asm lhs)
     (x86:mov x86:ecx x86:eax)
     (to-asm rhs)
     (x86:xchg x86:ecx x86:eax)
     (x86:mov x86:edx 0)
     (x86:idiv x86:ecx)
     (if remainder?
         (x86:mov x86:eax x86:edx)
         (x86:seqn)))]
   [(binop op assoc? lhs rhs)
    #|
     First, evaluate the LHS and save the result. Then
     evaluate the RHS and move the result to EBX. Then restore the LHS
     to EAX. Then do the math, storing the result in EAX.

     If the operation is associative, we can get rid of one mov
    |#
    (x86:seqn
     ;; LHS -> stack
     (to-asm lhs)
     (x86:mov x86:ebx x86:eax)

     ;; RHS -> eax
     (to-asm rhs)

     (if assoc?
         (x86:seqn)
         (x86:seqn
          (x86:xchg x86:ebx x86:eax)))
     
     ;; (op LHS RHS) -> eax
     (op x86:eax x86:ebx))]
   [(cmpop op lhs rhs)
    (x86:seqn
     (to-asm lhs)
     (x86:mov x86:edx x86:eax)
     (to-asm rhs)
     (x86:cmp x86:edx x86:eax)
     (op x86:al))]
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
