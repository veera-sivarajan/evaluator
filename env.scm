;; A frame is a list containing a pair :: ((vars) . (vals))
;; An environment is a list of frames (list of lists)
;; Current environment is the car of environment (first ele in list)
;; Enclosing environment is cdr of environment

(define (prev-envs envs) (cdr env))

(define (curr-env envs) (car env))

(define empty-env '())

(define (build-frame vars vals) (cons vars vals))

(define (frame-vars frame) (car frame))

(define (frame-vals frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))) 

(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (build-frame vars vals) base-env)
      (error "extend-env: Length of vars and vals not equal" vars vals)))
  
(define (lookup-variable var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (prev-envs env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Unbound variable" var)
        (let ((frame (curr-env env)))
          (scan (frame-vars frame) (frame-vals vals)))))
  (env-loop env)) 


(define (set-var-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (prev-envs env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Unbound variable -- SET" var)
        (let ((frame (curr-frame env)))
          (scan (frame-vars frame) (frame-vals frame)))))
  (env-loop env)) 
              
