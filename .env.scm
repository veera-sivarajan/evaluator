;; A frame is a list containing a pair :: ((vars) . (vals))
;; An environment is a list of frames (list of lists)
;; Current environment is the car of environment (first ele in list)
;; Enclosing environment is cdr of environment

(define (prev-envs envs) (cdr envs))

(define (curr-env envs) (car envs)) 

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
        (error "Unbound variable" var) ;FIXME: #35 tries to get the first
        (let ((frame (curr-env env)))  ; frame from first frame 
          (scan (frame-vars frame) (frame-vals frame)))))
  (env-loop env)) 

(define (set-var-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (prev-envs env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Unbound variable -- SET" var)
        (let ((frame (curr-env env)))
          (scan (frame-vars frame) (frame-vals frame)))))
  (env-loop env)) 

(define (define-variable! var val env)
  (let ((frame (curr-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame) (frame-vals frame)))) 

(define (remove-tree ele lis)
  (cond ((null? lis) '())
        ((pair? (car lis)) (cons (remove-tree ele (car lis))
                                 (remove-tree ele (cdr lis))))
        ((equal? (car lis) ele) (cdr lis))
        (else (cons (car lis) (remove-tree ele (cdr lis)))))) 

(define (remove-from-env var env)
  (let ((val (lookup-variable var env)))
    (cons (remove-tree var env)
          (remove-tree val env))))

(define (unbound! var env)
  (let ((new (remove-from-env var env)))
    (build-frame (caaar new) (cdadr new)))) 
