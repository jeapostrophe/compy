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
     ;; LHS -> stack
     (x86:push x86:eax)
     ;; RHS -> eax
     (to-asm rhs)
     ;; LHS -> edx
     (x86:pop x86:edx)
     ;; (op RHS LHS) -> eax
     (x86:imul x86:edx))]
   [(div remainder? lhs rhs)
    (x86:seqn
     (to-asm lhs)
     (x86:push x86:eax)
     (to-asm rhs)
     (x86:pop x86:ecx)
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
     (x86:push x86:eax)
     
     ;; RHS -> eax
     (to-asm rhs)

     (x86:pop x86:ebx)
     
     (if assoc?
         (x86:seqn)
         (x86:seqn
          (x86:xchg x86:ebx x86:eax)))
     
     ;; (op LHS RHS) -> eax
     (op x86:eax x86:ebx))]
   [(cmpop op lhs rhs)
    (x86:seqn
     (to-asm lhs)
     (x86:push x86:eax)
     (to-asm rhs)
     (x86:pop x86:edx)
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

(define (x86-op->racket-op o)
  (cond
   [(equal? o x86:add) +]
   [(equal? o x86:sub) -]
   [(equal? o x86:or) bitwise-ior]
   [(equal? o x86:xor) bitwise-xor]
   [(equal? o x86:and) bitwise-and]
   [(equal? o x86:inc) add1]
   [(equal? o x86:dec) sub1]
   [(equal? o x86:not) bitwise-not]
   [(equal? o x86:sete) =]
   [(equal? o x86:setg) >]
   [(equal? o x86:setge) >=]
   [(equal? o x86:setl) <]
   [(equal? o x86:setle) <=]))

(define bool->num
  (match-lambda
   [#t 1]
   [#f 0]))

(define interp
  (match-lambda
   [(cmpop op lhs rhs)
    (bool->num
     ((x86-op->racket-op op)
      (interp lhs)
      (interp rhs)))]
   [(div #f lhs rhs)
    (quotient (interp lhs)
              (interp rhs))]
   [(div #t lhs rhs)
    (remainder (interp lhs)
               (interp rhs))]
   [(mult lhs rhs)
    (* (interp lhs)
       (interp rhs))]
   [(binop op _ lhs rhs)
    ((x86-op->racket-op op)
     (interp lhs)
     (interp rhs))]
   [(unaop op lhs)
    ((x86-op->racket-op op)
     (interp lhs))]
   [(num b)
    b]))

(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]
  [interp (-> e? any/c)]))
