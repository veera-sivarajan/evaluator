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
