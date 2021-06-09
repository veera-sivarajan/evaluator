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
  
