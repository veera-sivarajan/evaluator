;; extract pieces of information from entered code
;; essentially defines the syntax of the language

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
