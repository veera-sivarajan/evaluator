;; new-env.scm 
;; A binding is a name value pair
;; A frame is a list of bindings

(define (println data)
  (newline)
  (display data)
  (newline)
  'done) 

(define empty-env '())

(define (new-frame vars vals)
  (if (and (null? vars) (null? vals))
      '()
      (cons (cons (car vars) (car vals))
            (new-frame (cdr vars) (cdr vals))))) 

(define (build-frame vars vals)
  (cons 'frame (new-frame vars vals)))

(define (binds frame) (cdr frame))

(define (frame-vars frame) (map car (binds frame))) 

(define (frame-vals frame) (map cdr (binds frame))) 

(define (curr-frame env) (car env))

(define (prev-frame env) (cdr env)) 

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (binds frame)))
  'done)

(define (extend-env vars vals base-env)
  (cons (build-frame vars vals) base-env)) 

(define (traverse-env proc var env #!optional val)
  (if (null? env)
      (error "Unbound variable" var)
      (let* ((frame (curr-frame env))
             (bind-list (binds frame)))
        (proc bind-list)))) 

(define (lookup-variable var env)
  (define (lookup bind-list)
    (cond ((null? bind-list) ;FIXME: Find better way to recurse
           (lookup-variable var (prev-frame env)))
          ((eq? var (caar bind-list)) (cdar bind-list))
          (else (lookup (cdr bind-list))))) 
  (traverse-env lookup var env)) 

(define (set-var-value! var val env)
  (define (set-var bind-list)
    (cond ((null? bind-list) (set-var-value! var val (prev-frame env)))
          ((eq? var (caar bind-list))
           (set-cdr! (car bind-list) val))
          (else (set-var (cdr bind-list)))))
  (traverse-env set-var var env val)) 

(define (define-variable! var val env)
  (let ((frame (curr-frame env)))
    (define (helper bind-list)
      (cond ((null? bind-list) (add-binding-to-frame! var val frame))
            ((eq? var (caar bind-list))
             (set-cdr! (car bind-list) val))
            (else (helper (cdr bind-list)))))
    (traverse-env helper var env val))) 
           


;; syntax.scm
;; extract pieces of information from entered code
;; essentially defines the syntax of the language
(println "syntax.scm")

(define (self-eval? exp) (or (number? exp) (string? exp))) 

;; (define (name? exp) (symbol? exp)) ;; variable name == symbol
(define name? symbol?) 

(define (oper=? exp name) (and (pair? exp) (eq? (car exp) name))) 

(define (quote? exp) (oper=? exp 'quote)) 

(define (quote-content exp) (car (cdr exp))) 

(define (assign? exp) (oper=? exp 'set!)) 

;; (set! x 2) -> x
(define (assign-var exp)
  (if (and (assign? exp) (name? (car (cdr exp))))
      (car (cdr exp))
      (error "Wrong expression for assign-var:" exp))) 
  
;; (set! x 2) -> 2
(define (assign-value exp)
  (if (assign? exp)
      (car (cdr (cdr exp)))
      (error "Wrong expression for assign-value:" exp))) 

(define (func? exp)
  (oper=? exp 'func)) ; func instead of define 

(define (func-name exp)
  (if (and (func? exp) (name? (car (car (cdr exp)))))
      (car (car (cdr exp))) ; (func (square n) (* n n)) -> square
      (error "Wrong expression for def-name:" exp)))

(define (func-value exp)
  (let ((f-params (cdr (car (cdr exp))))
        (body (cdr (cdr exp)))) ; func body can be multiple expressions
        (if (func? exp) ;(func (fact n) (if (= n 0) 1 (* n (fact (- n 1))))
            (build-make-proc f-params body) 
            (error "Wrong expression for func-value:" exp)))) 

(define (var? exp) (oper=? exp 'var))

(define (var-name exp)
  (if (and (var? exp) (name? (car (cdr exp)))) ; (var x (+ 1 (+ 2 3))) -> x
      (car (cdr exp))
      (error "Wrong expression for var-name:" exp))) 

(define (var-value exp)
  (if (var? exp)
      (car (cdr (cdr exp)))
      (error "Wrong expression for var-value:" exp))) 

;; make-proc is new lambda
(define (make-proc? exp) (oper=? exp 'make-proc)) 

(define (make-proc-params exp)
  (if (make-proc? exp)
      (car (cdr exp)) ; (lambda (num) (= num 5))
      (error "Wrong expression for make-proc-params:" exp))) 
  
(define (make-proc-body exp)
  (if (make-proc? exp)
      (cdr (cdr exp))
      (error "Wrong expression for make-proc-body:" exp))) 

(define (build-make-proc params body)
  (cons 'make-proc (cons params body))) 

(define (if? exp) (oper=? exp 'if))

(define (if-pred exp)
  (if (if? exp) 
      (car (cdr exp))
      (error "Wrong expression for if-pred:" exp)))

(define (if-conseq exp)
  (if (if? exp) ; (if (null? lis) '() (cons 1 2))
      (car (cdr (cdr exp)))
      (error "Wrong expression for if-conseq:" exp)))

(define (if-altern exp) ; proc for else clause
  (if (if? exp)
      (if (not (null? (cdr (cdr (cdr exp)))))
          (car (cdr (cdr (cdr exp))))
          'false)
      (error "Wrong expression for if-altern:" exp)))

(define (build-if pred conseq altern)
  (list 'if pred conseq altern)) 

(define (begin? exp) (oper=? exp 'begin))

(define (begin-exps exp)
  (if (begin? exp)
      (cdr exp)
      (error "Wrong expression for begin-exps:" exp)))

(define (build-begin seq)
  (cons 'begin seq)) 

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operand ops) (cdr ops))

(define (cond? exp) (oper=? exp 'cond))

(define (cond-clauses exp)
  (if (cond? exp)
      (cdr exp)
      (error "Wrong expression for cond-clauses:" exp))) 

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq)) 

(define (rest-exps seq) (cdr seq)) 

(define (sequence->exp seq)
  (cond ((null? seq) '())
        ((last-exp? seq) (first-exp seq))
        (else (build-begin seq)))) 

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses)) (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause should be last" clauses))
            (build-if (cond-predicate first)
                      (sequence->exp (cond-actions first))
                      (expand-clauses rest)))))) 

(define (let? exp) (oper=? exp 'let))

(define (let-binds exp)
  (if (let? exp)
      (car (cdr exp))
      (error "Wrong expression for let-binds:" exp)))

(define (let-body exp)
  (if (let? exp)
      (car (cdr (cdr exp)))
      (error "Wrong expression for let-body:" exp)))

(define (bind-vars binds) (map car binds)) 

(define (bind-exps binds) (map car (map cdr binds))) 

(define (build-combination params body exps)
  (cons (list 'lambda params body) exps)) 

(define (let->combination exps)
  (let ((binds (let-binds exps)))
    (build-combination (bind-vars binds)
                       (let-body exps)
                       (bind-exps binds))))

(define (false? exp) (eq? exp false)) 

(define (true? exp) (not (false? exp))) 

(define (build-procedure params body env)
  (list 'proc params body env)) 

(define (compound-proc? p) (oper=? p 'proc)) 

(define (proc-params p)
  (if (compound-procedure? p)
      (car (cdr p))
      (error "Wrong expression for proc-params:" p)))

(define (proc-body p)
  (if (compound-procedure? p)
      (car (cdr (cdr p)))
      (error "Wrong expression for proc-body:" p)))

(define (proc-env p)
  (if (compound-procedure? p)
      (car (cdr (cdr (cdr p))))
      (error "Wrong expression for proc-env:" p)))
;; primitives.scm

(define (foldl oper init lis) 
  (if (null? lis)
      init
      (foldl oper (oper init (car lis)) (cdr lis)))) 

(define (foldr oper init lis)
  (if (null? lis)
      init
      (oper (car lis) (foldr oper init (cdr lis))))) 

(define (v-map proc lis)
  (foldr (lambda (first result) (cons (proc first) result)) '() lis)) 

(define (v-filter pred lis)
  (define (filter-helper first result)
    (if (pred first)
        (cons first result)
        result))
  (foldr filter-helper '() lis)) 

(define (v-reverse lis)
  (foldl (lambda (init first) (cons first init)) '() lis)) 

(define (v-length lis)
  (foldl (lambda (init first) (+ init 1)) 0 lis)) 

(define (v-append list-a list-b)
  (foldr cons list-b list-a)) 

(define (v-list-ref lis index)
  (if (and (< index (v-length lis)) (>= index 0))
      (if (= index 0)
          (car lis)
          (v-list-ref (cdr lis) (- index 1)))
      (error "Index out of range" index)))

(define (v-for-each proc lis)
  (if (null? lis)
      'done
      (begin (proc (car lis))
             (v-for-each proc (cdr lis))))) 

(define (v-remove ele lis)
  (define (remove-helper first init)
    (if (equal? first ele)
        init
        (cons first init)))
  (foldr remove-helper '() lis)) 

(define (v-remove-first ele lis)
  (if (not (null? lis))
      (if (equal? (car lis) ele)
          (cdr lis)
          (cons (car lis) (v-remove-first ele (cdr lis))))
      (error "Not in list ele:" ele))) 

(define (v-and exps)
  (cond ((false? (car exps)) 'false)
        ((null? exps) 'true)
        (else (v-and (cdr exps))))) 

(define (v-or exps)
  (cond ((null? exps) 'false)
        ((not (false? (car exps))) 'true)
        (else (v-or (cdr exps))))) 

(define (range num)
  (define (range-helper start)
    (if (= start (- num 1))
        (cons start '()) 
        (cons start (range-helper (+ start 1)))))
  (range-helper 0)) 

(define (my-abs num)
  (if (< num 0)
      (* -1 num)
      num))

(define (v-even? num) (= (remainder num 2) 0)) 

(define (v-odd? num) (not (v-even? num))) 

(define (println data)
  (newline)
  (display data)
  (newline)
  'done) 

;TODO: Rename keywords to make fancier names
;; 1. cons -> pair
;; 2. car  -> head
;; 3. cdr  -> tail

(define (primitive-proc? proc) (oper=? proc 'primitive)) 

(define primitive-procs
  (list (list 'foldl foldl)
        (list 'foldr foldr)
        (list 'map v-map)
        (list 'filter v-filter)
        (list 'reverse v-reverse)
        (list 'length v-length)
        (list 'append v-append)
        (list 'list-ref v-list-ref)
        (list 'for-each v-for-each)
        (list 'remove 'v-remove)
        (list 'remove-first 'v-remove-first)
        (list 'head car)
        (list 'tail cdr)
        (list 'pair cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)))

(define (primitive-procs-names) (map car primitive-procs)) 

(define (primitive-proc-vals)
  (map (lambda (proc) (cons 'primitive (cadr proc))) primitive-procs)) 

(define (name->primitive proc) (car (cdr proc)))

(define (apply-primitive-proc proc args)
  (apply-in-scheme (name->primitive proc) args)) 
;; eval.scm

(define (first-exp seq) (car seq)) 

(define (println data)
  (newline)
  (display data)
  (newline))

(define (eval exp env)
  (cond ((self-eval? exp) exp)
        ((name? exp) (lookup-variable exp env))
        ((quote? exp) (quote-content exp))
        ((assign? exp) (eval-assign exp env)) 
        ((func? exp) (eval-func exp env)) ; TODO
        ((if? exp) (eval-if exp env))
        ((make-proc? exp) (build-procedure (make-proc-params exp)
                                           (make-proc-body exp)
                                           env))
        ((begin? exp) (eval-sequence (begin-exps exp) env)) ; TODO
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))  
        (else (error "Unknown expression -- EVAL" exp)))) 

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operand exp) env)))) 

(define (eval-if exp env)
  (if (true? (eval (if-pred exp) env))
      (eval (if-conseq exp) env)
      (eval (if-altern exp) env))) 
             
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
         (else (eval (first-exp exps) env)
               (eval-sequence (rest-exps exp) env)))) 

(define (eval-assign exp env)
  (set-var-value! (assign-var exp)
                  (eval (assign-value exp) env)
                  env)
  'done)

(define (eval-func exp env)
  (define-variable! (func-name exp) (eval (func-value exp) env) env)
  'done) 
    
(define (apply proc args)
  (cond ((primitive-proc? proc) (apply-primitive-proc proc args))
        ((compound-proc? proc)
         (eval-sequence (proc-body proc)
                        (extend-env (proc-params proc)
                                    args
                                    (proc-env proc))))
        (else (error "Unknown proc type -- APPLY" proc)))) 
        

    
  

    
    
;; main.scm

(define (println data)
  (newline)
  (display data)
  (newline)
  'done) 

(define (setup-env)
  (let ((init-env (extend-env (primitive-procs-names)
                              (primitive-proc-vals)
                              empty-env)))
    (define-variable! 'true true init-env)
    (define-variable! 'false false init-env)
    init-env))

(define global-env (setup-env)) 

(define apply-in-scheme apply) 

(define input-prompt ">> ")

(define output-prompt "Value:")

(define (user-print obj)
  (if (compound-procedure? obj)
      (display (list 'compound-procedure
                     (proc-params obj)
                     (proc-body obj)
                     '<procedure-env>))
      (display obj))) 

(define (driver-loop)
  (println input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-env)))
      (println output-prompt)
      (user-print output)))
  (driver-loop)) 
