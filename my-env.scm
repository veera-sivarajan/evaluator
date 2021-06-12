;; Implementing the environment for a Lisp Interpreter
;; A frame is a list with any number of lists representing bindings

(define empty-env '()) 

(define (new-frame vars vals)
  (if (and (null? vars) (null? vals))
      '()
      (cons (cons (car vars) (car vals))
                  (new-frame (cdr vars) (cdr vals)))))

(define (build-frame vars vals)
  (cons 'frame (new-frame vars vals)))

(define (binds frame) (cdr frame))  

(define (frame-vars frame) (map car (binds frame)))

(define (frame-vals frame) (map cdr (binds frame)))

(define (curr-frame env) (car env))

(define (prev-frame env) (cdr env)) 

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (binds frame)))
  'done) 

(define (extend-env vars vals base-env)
  (cons (build-frame vars vals) base-env)) 

(define (lookup-variable var env)
  (define (scan bind-list)
    (cond ((equal? bind-list '())
           (lookup-variable var (prev-frame env)))
          ((eq? var (caar bind-list)) (cdar bind-list))
          (else (scan (cdr bind-list)))))
  (if (equal? env empty-env)
      (error "Unbound variable" var)
      (let* ((frame (car env))
             (bind-list (binds frame)))
        (scan bind-list)))) 

(define (set-var-value! var val env)
  (define (scan bind-list)
    (cond ((equal? bind-list '())
           (set-var-value! var val (prev-frame env)))
          ((eq? var (caar bind-list)) (set-cdr! (car bind-list) val))
          (else (scan (cdr bind-list)))))
  (if (equal? env empty-env)
      (error "Unbound variable" var)
      (let* ((frame (car env))
             (bind-list (binds frame)))
        (scan bind-list)))) 

(define (define-variable! var val env)
  (let ((frame (curr-frame env)))
    (define (scan bind-list)
      (cond ((null? bind-list) (add-binding-to-frame! var val frame))
            ((eq? var (caar bind-list)) (set-cdr! (car bind-list) val))
            (else (scan (cdr bind-list)))))
    (scan (binds frame))))
                
