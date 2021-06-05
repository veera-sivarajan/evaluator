;; extract pieces of information from entered code
;; essentially defines the syntax of the language

(define (self-eval? exp) (or (number? exp) (string? exp))) 

(define (var-name? exp) (symbol? exp)) ;; variable name == symbol

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

(define (def? exp)
  (oper=? exp 'def)) ; def instead of define 

(define (def-name exp)
  (if (def? exp)
      (if (var-name? (car (cdr exp))) 
          (car (cdr exp)) ; (def x 2) -> x
          (car (car (cdr exp)))) ; (def (square n) (* n n)) -> square
      (error "Wrong expression for def-name:" exp))) 

(define (def-value exp)
  (define (formal-params exp) (cdr (car (cdr exp)))) 
  (if (def? exp)
      (if (var-name? (car (cdr exp)))
          (car (cdr (cdr exp))) ; (def x (+ 1 2)) -> (+ 1 2)
          (make-lambda (formal-params exp) (cdr (cdr exp))))
      (error "Wrong expression for def-value:" exp))) 
      
