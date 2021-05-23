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

