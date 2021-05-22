(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definiton exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp)))) 

(define (my-apply proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence (procedure-body proc)
                        (extend-environment
                         (procedure-parameters proc)
                         args
                         (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" proc)))) 

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-of-operands exps) env)))) 

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env)) 
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env))) 

(define (eval-seqs exps env) ;; return value == value of final exp
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-seqs (rest-exps exps) env)))) 

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)) 

(define (eval-definition exp env)
  (my-define-variable! (definition-variable exp)
                       (eval (definition-value exp) env)
                       env)
  'ok) 
    
    

