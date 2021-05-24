(define (enclosing-env env) (cdr env)) 

(define (first-frame env) (car env)) 

(define empty-env '()) 

(define (make-frame var-list val-list)
  (cons var-list val-list)) 

(define (frame-vars f)
  (car f))

(define (frame-vals f)
  (cdr f)) 

(define (bind-var! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))) 

(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Variables and values do not match" vars vals))) 

(define (value-lookup var env)
  (define (env-loop env)
    (define (scan var-list val-list)
      (cond ((null? vars) (env-loop enclosing-env env))
            ((eq? var (car var-list)) (car val-list))
            (else (scan (cdr var-list) (cdr val-list)))))
    (if (eq? env empty-env)
        (error "Unbound variable" var)
        (scan (frame-vars (first-frame env))
              (frame-vals (first-frame env)))))
    (env-loop env)) 
      
