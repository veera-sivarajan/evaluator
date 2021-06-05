;; extract pieces of information from entered code
;; essentially defines the syntax of the language

(define (self-eval? exp) (or (number? exp) (string? exp))) 

(define (var-name? exp) (symbol? exp)) ;; variable name == symbol

(define (oper=? exp name) (and (pair? exp) (eq? (car exp) name))) 

(define (assign? exp) (oper=? exp 'set!)) 

(define (quote? exp) (oper=? exp 'quote)) 

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
      
