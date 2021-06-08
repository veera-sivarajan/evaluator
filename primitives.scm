(define (foldl oper init lis) 
  (if (null? lis)
      init
      (foldl oper (oper init (car lis)) (cdr lis)))) 

(define (foldr oper init lis)
  (if (null? lis)
      init
      (oper (car lis) (foldr oper init (cdr lis))))) 

(define (v-map proc lis)
  (foldr (lambda (first result) (cons (proc first) result)) '() lis)) 

(define (v-filter pred lis)
  (define (filter-helper first result)
    (if (pred first)
        (cons first result)
        result))
  (foldr filter-helper '() lis)) 

(define (v-reverse lis)
  (foldl (lambda (init first) (cons first init)) '() lis)) 

(define (v-length lis)
  (foldl (lambda (init first) (+ init 1)) 0 lis)) 

(define (v-append list-a list-b)
  (foldr (lambda (first init) (cons first init)) list-b list-a)) 

(define (v-list-ref lis index)
  (if (and (< index (v-length lis)) (>= index 0))
      (if (= index 0)
          (car lis)
          (v-list-ref (cdr lis) (- index 1)))
      (error "Index out of range" index))) 

