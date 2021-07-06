;; main.scm

(define (println data)
  (newline)
  (display data)
  (newline)
  'done) 

(define (setup-env)
  (let ((init-env (extend-env (primitive-procs-names)
                              (primitive-proc-vals)
                              empty-env)))
    (define-variable! 'true true init-env)
    (define-variable! 'false false init-env)
    init-env))

(define global-env (setup-env)) 

(define apply-in-scheme apply) 

(define input-prompt ">> ")

(define output-prompt "Value:")

(define (user-print obj)
  (if (compound-procedure? obj)
      (display (list 'compound-procedure
                     (proc-params obj)
                     (proc-body obj)
                     '<procedure-env>))
      (display obj))) 

(define (driver-loop)
  (println input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-env)))
      (println output-prompt)
      (user-print output)))
  (driver-loop)) 
