#lang racket/base
(require racket/contract
         racket/function
         racket/list
         racket/match
         (prefix-in x86: "asm.rkt"))

#|
 p = (d ... fe)
   | e
 d = (define (<id> <id> ...4) fe)
 fe = e
    | (app <id> e ...4)
 e = <id>
   | <num>
   | (if0 e e e)
   | (* e e)
   | (quotient e e)
   | (remainder e e)
   | (cmpop e e)
   | (binop e e)
   | (unaop e)
|#

(struct e () #:prefab)

(struct program e (defs fe) #:prefab)

(struct def (fun args body) #:prefab)

(struct fe () #:prefab)
(struct call fe (fun args) #:prefab)

(struct ee fe () #:prefab)
(struct id ee (s) #:prefab)
(struct num ee (n) #:prefab)
(struct if0 ee (tst tru fal) #:prefab)
(struct mult ee (lhs rhs) #:prefab)
(struct div ee (remainder? lhs rhs) #:prefab)
(struct cmpop ee (op lhs rhs) #:prefab)
(struct binop ee (op assoc? lhs rhs) #:prefab)
(struct unaop ee (op lhs) #:prefab)

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

(define (parse p)
  (cond
   [(parse-e p) =>
    (lambda (e)
      (program empty e))]
   [else
    (match p
      [(list d ... fe)
       (program (map parse-def d)
                (parse-fe fe))]
      [_ #f])]))

(define parse-def
  (match-lambda
   [(list 'define (list (? symbol? f) (? symbol? arg) ...) body)
    (if ((length arg) . <= . 4)
        (def f arg (parse-fe body))
        (error 'parse-def "Too many arguments"))]
   [_
    #f]))

(define parse-fe
  (match-lambda
   [(list 'app (? symbol? f) arg ...)
    (if ((length arg) . <= . 4)
        (call f (map parse-e arg))
        (error 'parse-fe "Too many arguments"))]
   [e-se
    (parse-e e-se)]
   [_
    #f]))

(define parse-e
  (match-lambda
   [(? symbol? s)
    (id s)]
   [(list 'if0 tst tru fal)
    (if0 (parse-e tst)
         (parse-e tru)
         (parse-e fal))]
   [(list '* lhs rhs)
    (mult (parse-e lhs) (parse-e rhs))]
   [(list 'quotient lhs rhs)
    (div #f (parse-e lhs) (parse-e rhs))]
   [(list 'remainder lhs rhs)
    (div #t (parse-e lhs) (parse-e rhs))]
   [(list (and (? symbol?)
               (app (lambda (s)
                      (hash-ref binops s #f))
                    (cons opcode assoc?)))
          lhs
          rhs)
    (binop opcode assoc?
           (parse-e lhs)
           (parse-e rhs))]
   [(list (and (? symbol?)
               (app (lambda (s)
                      (hash-ref cmpops s #f))
                    (and (not #f)
                         opcode)))
          lhs rhs)
    (cmpop opcode
           (parse-e lhs)
           (parse-e rhs))]
   [(list (and (? symbol?)
               (app (lambda (s)
                      (hash-ref unaops s #f))
                    (and (not #f)
                         opcode)))
          lhs)
    (unaop opcode
           (parse-e lhs))]
   [(? byte? b)
    (num b)]
   [_
    #f]))

(define (to-asm p)
  (x86:seqn
   (x86:comment (format "~v" p))
   (match p
     [(program defs fe)
      (define fun->label
        (for/hasheq ([d (in-list defs)])
                    (define f (def-fun d))
                    (define lab (x86:make-label f))
                    (values f
                            lab)))
      (define end-lab (x86:make-label 'end))
      (x86:seqn
       (to-asm-fe fun->label end-lab empty fe)
       (apply x86:seqn
              (for/list ([d (in-list defs)])
                        (to-asm-def fun->label (hash-ref fun->label (def-fun d)) end-lab d)))
       (x86:label-mark end-lab))])))

(define (to-asm-def fun->label lab end-lab d)
  (x86:seqn
   (x86:comment (format "~v" d))
   (match d
     [(def fun args body)
      (x86:seqn
       (x86:label-mark lab)
       (to-asm-save-ids args)
       (to-asm-fe fun->label end-lab args body))])))

(define-syntax-rule (when-x86 cond e ...)
  (if cond
      (x86:seqn e ...)
      (x86:seqn)))


(define (to-asm-save-ids ids)
  (x86:seqn
   (x86:comment (format "save-ids ~v" ids))
   ;; []
   (when-x86 ((length ids) . >= . 4)
             (x86:push x86:edx))
   ;; [arg4]
   (when-x86 ((length ids) . >= . 3)
             (x86:push x86:ecx))
   ;; [arg3 arg4]
   (when-x86 ((length ids) . >= . 2)
             (x86:push x86:ebx))
   ;; [arg2 arg3 arg4]
   (when-x86 ((length ids) . >= . 1)
             (x86:push x86:eax))
   ;; [arg1 arg2 arg3 arg4]
   ))

(define (to-asm-trash-ids ids)
  (x86:seqn
   (x86:comment (format "trash-ids ~v" ids))
   (x86:add x86:esp (* 4 (length ids)))))

(define (to-asm-fe fun->label end-lab arg-ids fe)
  (x86:seqn
   (x86:comment (format "fe ~v" fe))
   (match fe
     [(call fun args)
      (x86:seqn
       ;; *, *, *, * :: [in-arg ...]
       (when-x86 ((length args) . >= . 4)
                 (to-asm-e arg-ids (fourth args))
                 (x86:push x86:eax))
       ;; arg4, *, *, * :: [arg4 in-arg ...]      
       (when-x86 ((length args) . >= . 3)
                 (to-asm-e (append (make-list (- (length args) 3) #f)
                                   arg-ids)
                           (third args))
                 (x86:push x86:eax))
       ;; arg3, *, *, * :: [arg3 arg4 in-arg ...]
       (when-x86 ((length args) . >= . 2)
                 (to-asm-e (append (make-list (- (length args) 2) #f)
                                   arg-ids)
                           (second args))
                 (x86:push x86:eax))
       ;; arg2, *, *, * :: [arg2 arg3 arg4 in-arg ...]
       (when-x86 ((length args) . >= . 1)
                 (to-asm-e (append (make-list (- (length args) 1) #f)
                                   arg-ids)
                           (first args)))
       ;; arg1, *, *, * :: [arg2 arg3 arg4 in-arg ...]
       (when-x86 ((length args) . >= . 2)
                 (x86:pop x86:ebx))
       ;; arg1, arg2, *, * :: [arg3 arg4 in-arg ...]
       (when-x86 ((length args) . >= . 3)
                 (x86:pop x86:ecx))
       ;; arg1, arg2, arg3, * :: [arg4 in-arg ...]
       (when-x86 ((length args) . >= . 4)
                 (x86:pop x86:edx))
       ;; arg1, arg2, arg3, arg4 :: [in-arg ...]      
       (to-asm-trash-ids arg-ids)      
       (x86:jmp
        (hash-ref fun->label fun
                  (lambda () (error 'to-asm "Unknown function: ~e" fun)))))]
     [e
      (x86:seqn
       (to-asm-e arg-ids e)
       (to-asm-trash-ids arg-ids)
       (x86:jmp end-lab))])))

(define (to-asm-e args e)
  (x86:seqn
   (x86:comment (format "e ~v" e))
   (match e
     [(id s)
      (define arg-n
        (let/ec esc
          (for ([n (in-naturals)]
                [a (in-list args)])
               (when (equal? a s)
                 (esc n)))
          (error 'to-asm-e "Unbound identifier ~e" s)))
      (x86:seqn
       (x86:mov x86:eax (x86:esp+ (* 4 arg-n))))]
     [(if0 tst tru fal)
      (define false-label (x86:make-label 'if0false))
      (define after-label (x86:make-label 'if0after))
      (x86:seqn
       (to-asm-e args tst)
       (x86:cmp x86:eax 0)
       (x86:jne false-label)
       (to-asm-e args tru)
       (x86:jmp after-label)
       (x86:label-mark false-label)
       (to-asm-e args fal)
       (x86:label-mark after-label))]
     [(mult lhs rhs)
      (x86:seqn
       ;; LHS -> eax
       (to-asm-e args lhs)
       ;; LHS -> stack
       (x86:push x86:eax)
       ;; RHS -> eax
       (to-asm-e (list* #f args) rhs)
       ;; LHS -> edx
       (x86:pop x86:edx)
       ;; (op RHS LHS) -> eax
       (x86:imul x86:edx))]
     [(div remainder? lhs rhs)
      (x86:seqn
       (to-asm-e args lhs)
       (x86:push x86:eax)
       (to-asm-e (list* #f args) rhs)
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
       (to-asm-e args lhs)
       (x86:push x86:eax)

       ;; RHS -> eax
       (to-asm-e (list* #f args) rhs)

       (x86:pop x86:ebx)
       (if assoc?
           (x86:seqn)
           (x86:seqn
            (x86:xchg x86:ebx x86:eax)))
       
       ;; (op LHS RHS) -> eax
       (op x86:eax x86:ebx))]
     [(cmpop op lhs rhs)
      (x86:seqn
       (to-asm-e args lhs)
       (x86:push x86:eax)
       (to-asm-e (list* #f args) rhs)
       (x86:pop x86:edx)
       (x86:cmp x86:edx x86:eax)
       (op x86:al))]
     [(unaop op lhs)
      (x86:seqn
       ;; LHS -> eax
       (to-asm-e args lhs)
       ;; (op LHS) -> eax
       (op x86:eax))]
     [(num b)
      (x86:seqn
       (x86:mov x86:eax b))])))

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
   [(program defs fe)
    (define fun->lam
      (for/hasheq ([d (in-list defs)])
       (match-define (def f as fe) d)
       (values f
               (lambda vs
                 (interp-fe fun->lam
                            (for/hasheq ([a (in-list as)]
                                         [v (in-list vs)])
                             (values a v))
                            fe)))))
    (interp-fe fun->lam (hasheq) fe)]))

(define (interp-fe fun->lam arg->val fe)
  (match fe
   [(call fun es)
    (apply (hash-ref fun->lam fun)
           (map (curry interp-e arg->val) es))]
   [e
    (interp-e arg->val e)]))

(define (interp-e arg->val e)
  (match e
    [(id s)
     (hash-ref arg->val s)]
    [(if0 tst tru fal)
     (if (zero? (interp-e arg->val tst))
         (interp-e arg->val tru)
         (interp-e arg->val fal))]
    [(cmpop op lhs rhs)
     (bool->num
      ((x86-op->racket-op op)
       (interp-e arg->val lhs)
       (interp-e arg->val rhs)))]
    [(div #f lhs rhs)
     (quotient (interp-e arg->val lhs)
               (interp-e arg->val rhs))]
    [(div #t lhs rhs)
     (remainder (interp-e arg->val lhs)
                (interp-e arg->val rhs))]
    [(mult lhs rhs)
     (* (interp-e arg->val lhs)
        (interp-e arg->val rhs))]
    [(binop op _ lhs rhs)
     ((x86-op->racket-op op)
      (interp-e arg->val lhs)
      (interp-e arg->val rhs))]
    [(unaop op lhs)
     ((x86-op->racket-op op)
      (interp-e arg->val lhs))]
    [(num b)
     b]))

(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]
  [interp (-> e? any/c)]))
