;; self eval
(define (self-eval? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false))) 

;; variable
(define (variable? exp)
  (symbol? exp)) 

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false)) 

;; quotes
(define (quoted? exp)
  (tagged-list? exp 'quote)) 

(define (text-of-quotation exp)
  (car (cdr exp))) 

;; assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (car (cdr exp)))

(define (assignment-value exp)
  (car (cdr (cdr exp)))) 

;;define
(define (definition? exp)
  (tagged-list? exp 'def)) ;;slight modification of syntax: define -> def

(define (definition-variable exp)
  (if (symbol? (car (cdr exp)))
      (car (cdr exp)) ; extract var from (define var exp) 
      (car (car (cdr exp))))) ; extract var from (define (var par1 par2))


