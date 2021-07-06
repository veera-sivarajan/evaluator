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
      (car (car (cdr exp)))) ; (func (square n) (* n n)) -> square
      (error "Wrong expression for def-name:" exp)) 

(define (func-value exp)
  (let ((f-params (cdr (car (cdr exp))))
        (body (cdr (cdr exp)))) ; func body can be multiple expressions
        (if (func? exp) ;(func (fact n) (if (= n 0) 1 (* n (fact (- n 1))))
            (make-proc f-params body) 
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

(define (operand exp) (cdr exp))

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
