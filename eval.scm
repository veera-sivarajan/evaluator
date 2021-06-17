(define (println data)
  (newline)
  (display data)
  (newline))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((name? exp) (lookup-variable exp env))
        ((quote? exp) (quote-content exp))
        ((assign? exp) (eval-assign exp env)) ;TODO
        ((func? exp) (eval-func exp env)) ; TODO
        ((if? exp) (eval-if exp env))
        ((make-proc? exp) (build-procedure (make-proc-params exp)
                                           (make-proc-body exp)
                                           env))
        ((begin? exp) (eval-sequence (begin-exps exp) env)) ; TODO
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))) ; TODO
        (else (error "Unknown expression -- EVAL" exp)))) 
