;; eval.scm

(define (first-exp seq) (car seq)) 

(define (println data)
  (newline)
  (display data)
  (newline))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((name? exp) (lookup-variable exp env))
        ((quote? exp) (quote-content exp))
        ((assign? exp) (eval-assign exp env)) 
        ((func? exp) (eval-func exp env)) ; TODO
        ((if? exp) (eval-if exp env))
        ((make-proc? exp) (build-procedure (make-proc-params exp)
                                           (make-proc-body exp)
                                           env))
        ((begin? exp) (eval-sequence (begin-exps exp) env)) ; TODO
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))  
        (else (error "Unknown expression -- EVAL" exp)))) 

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operand exp) env)))) 

(define (eval-if exp env)
  (if (true? (eval (if-pred exp) env))
      (eval (if-conseq exp) env)
      (eval (if-altern exp) env))) 
             
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
         (else (eval (first-exp exps) env)
               (eval-sequence (rest-exps exp) env)))) 

(define (eval-assign exp env)
  (set-var-value! (assign-var exp)
                  (eval (assign-value exp) env)
                  env)
  'done)

(define (eval-func exp env)
  (define-variable! (func-name exp) (eval (func-value exp) env) env)
  'done) 
    
(define (apply proc args)
  (cond ((primitive-proc? proc) (apply-primitive-proc proc args))
        ((compound-proc? proc)
         (eval-sequence (proc-body proc)
                        (extend-env (proc-params proc)
                                    args
                                    (proc-env proc))))
        (else (error "Unknown proc type -- APPLY" proc)))) 
        

    
  

    
    
