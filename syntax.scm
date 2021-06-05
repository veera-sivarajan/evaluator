;; extract pieces of information from entered code
;; essentially defines the syntax of the language

(define (self-eval? exp) (or (number? exp) (string? exp))) 

;; (define (var-name? exp) (symbol? exp)) ;; variable name == symbol

(define (oper=? exp name) (and (pair? exp) (eq? (car exp) name))) 

(define (quote? exp) (oper=? exp 'quote)) 

(define (quote-content exp) (car (cdr exp))) 

(define (assign? exp) (oper=? exp 'set!)) 

;; (set! x 2) -> x
(define (assign-var exp)
  (if (assign? exp)
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
  (if (func? exp)
      (car (car (cdr exp)))) ; (func (square n) (* n n)) -> square
      (error "Wrong expression for def-name:" exp)) 

(define (func-value exp)
  (let ((f-params (cdr (car (cdr exp))))
        (body (cdr (cdr exp)))) ; func body can be multiple expressions
        (if (func? exp) ;(func (fact n) (if (= n 0) 1 (* n (fact (- n 1))))
            (make-lambda f-params body) 
            (error "Wrong expression for func-value:" exp)))) 

(define (var? exp) (oper=? exp 'var))

(define (var-name exp)
  (if (var? exp) ; (var x (+ 1 (+ 2 3)))
      (car (cdr exp))
      (error "Wrong expression for var-name:" exp))) 

(define (var-value exp)
  (if (var? exp)
      (car (cdr (cdr exp)))
      (error "Wrong expression for var-value:" exp))) 
