;; Implementing the environment for a Lisp Interpreter
;; A frame is a list with any number of lists representing bindings

(define empty-env '(frame)) 

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

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (binds frame)))
  'done) 

  
