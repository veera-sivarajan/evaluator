(define (true? x) ; anything not false is true
  (not (eq? x false))) 

(define (false? x)
  (eq? x false)) 

(define (make-procedure param body env)
  (list 'procedure param body env)) 

(define (compound-proc? p)
  (cmp-car p 'procedure)) 

(define (proc-params p) (cadr p)) 

(define (proc-body p) (caddr p)) 

(define (proc-env p) (cadddr p)) 
