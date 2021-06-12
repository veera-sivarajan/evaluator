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


