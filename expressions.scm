;; self eval
(define (self-eval? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false))) 

;; variable
(define (variable? exp)
  (symbol? exp)) 

(define (cmp-car exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false)) 

;; quotes
(define (quoted? exp)
  (cmp-car exp 'quote)) 

(define (text-of-quotation exp)
  (car (cdr exp))) 

;; assignment
(define (assignment? exp)
  (cmp-car exp 'set!))

(define (assignment-variable exp)
  (car (cdr exp)))

(define (assignment-value exp)
  (car (cdr (cdr exp)))) 

;;define
(define (definition? exp)
  (cmp-car exp 'def)) ;;slight modification of syntax: define -> def

(define (definition-variable exp)
  (if (symbol? (car (cdr exp)))
      (car (cdr exp)) ; extract var from (define var exp) 
      (car (car (cdr exp))))) ; extract var from (define (var par1 par2))

(define (definition-value exp)
  (if (symbol? (car (cdr exp)))
      (car (cdr (cdr exp)))
      (make-lambda (cdr (car (cdr exp))) ; (define var (lambda (p1 .. pn)
                   (cdr (cdr exp))))) ;         body))

;; lambda
(define (lambda? exp) (cmp-car exp 'func)) ; use func instead of lambda 

(define (lambda-parameters exp) (car (cdr exp)))

(define (lambda-body exp) (cdr (cdr exp))) 

(define (make-lambda params body)
  (cons 'lambda (cons params body))) 

;; if statements
(define (if? exp) (cmp-car exp 'if))

(define (if-predicate exp) (car (cdr exp))) 

(define (if-conseq exp) (car (cdr (cdr exp)))) ; if true

(define (if-alter exp)
  (if (not (null? (cdr (cdr (cdr exp)))))
      (car (cdr (cdr (cdr exp))))
      'false)) 

(define (make-if pred conseq alter)
  (list 'if pred conseq alter)) 

;; begin
(define (begin? exp) (cmp-car exp 'begin)) 

(define (begin-exps exp) (cdr exp)) 

(define (last-exp? seq) (null? (cdr seq))) 

(define (first-exp seq) (car seq)) 

(define (rest-exp seq) (cdr seq)) 

(define (make-begin seq) (cons 'begin seq)) 

(define (seq->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq)))) 

;; apply
(define (application? exp) (pair? exp)) ;; pair that is not any of above

;; (define (application? exp) (cmp-car exp 'call))

(define (operator exp) (car exp)) ;; (fact 3) -> operator == fact

(define (operands exp) (cdr exp)) ;; operand == 3

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operand ops) (cdr ops)) 
