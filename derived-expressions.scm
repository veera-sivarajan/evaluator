;; cond 
;; cond is implemented like a macro using if
(define (cond? exp) (cmp-car exp 'cond)) 

(define (cond-clauses exp) (cdr exp))

(define (cond-pred clause) (car clause))

(define (is-else? clause) (cmp-car clause 'else)) 

(define (cond-actions clause) (cdr clause)) 

(define (cond->if exp) (expand-clauses (cond-clauses exp))) 

(define (expand-clauses clauses)
  (if (null? clauses) ; no else clause
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (and (is-else? first) (null? rest)) ; else can have a seq of 
            (seq->exp (cond-actions first))     ; actions
            (make-if (cond-pred first) 
                     (seq->exp (cond-actions first))
                     (expand-clauses rest)))))) 

(define (and? exp) (cmp-car exp 'and))

(define (and-pred exp) (cdr exp)) 

(define (eval-and preds)
  (if (null? preds)
      true
      (let ((first (car preds))
            (rest (cdr preds)))
        (if (false? first)
            false
            (eval-and (cdr preds)))))) 

(define (or? exp) (cmp-car exp 'or))

(define (or-pred exp) (cdr exp))

(define (eval-or preds)
  (if (null? preds)
      false
      (let ((first (car preds))
            (rest (cdr preds)))
        (if (true? first)
            true
            (eval-or (cdr preds)))))) 

;; let
(define (let? exp) (cmp-car exp 'let))

(define (let-values exp) (cadr exp)) 

(define (let-body exp) (cddr exp)) 

(define (let-vars exp) (map car exp)) 

(define (let-exps exp) (map cadr exp))

(define (let->combi exp)
  (append (cons (make-lambda (let-vars (let-values exp))
                             (let-body exp)) '())
          (let-exps (let-values exp)))) 

    
    
