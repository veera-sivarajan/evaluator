;; A binding is a name value pair
;; A frame is a list of bindings

(define (println data)
  (newline)
  (display data)
  (newline)
  'done) 

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

(define (abstract proc var env #!optional val)
  (if (null? env)
      (error "Unbound variable" var)
      (let* ((frame (curr-frame env))
             (bind-list (binds frame)))
        (proc bind-list)))) 

(define (lookup-variable var env)
  (define (lookup bind-list)
    (cond ((null? bind-list) ;FIXME: Find better way to recurse
           (lookup-variable var (prev-frame env)))
          ((eq? var (caar bind-list)) (cdar bind-list))
          (else (lookup (cdr bind-list))))) 
  (abstract lookup var env)) 

(define (set-var-value! var val env)
  (define (set-var bind-list)
    (cond ((null? bind-list) (set-var-value! var val (prev-frame env)))
          ((eq? var (caar bind-list))
           (begin (set-cdr! (car bind-list) val)
                  'done))
          (else (set-var (cdr bind-list)))))
  (abstract set-var var env val)) 
           


