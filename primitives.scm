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
  (foldr cons list-b list-a)) 

(define (v-list-ref lis index)
  (if (and (< index (v-length lis)) (>= index 0))
      (if (= index 0)
          (car lis)
          (v-list-ref (cdr lis) (- index 1)))
      (error "Index out of range" index)))

(define (v-for-each proc lis)
  (if (null? lis)
      'done
      (begin (proc (car lis))
             (v-for-each proc (cdr lis))))) 

(define (v-remove ele lis)
  (define (remove-helper first init)
    (if (equal? first ele)
        init
        (cons first init)))
  (foldr remove-helper '() lis)) 

(define (v-remove-first ele lis)
  (if (not (null? lis))
      (if (equal? (car lis) ele)
          (cdr lis)
          (cons (car lis) (v-remove-first ele (cdr lis))))
      (error "Not in list ele:" ele))) 

(define (v-and exps)
  (cond ((false? (car exps)) 'false)
        ((null? exps) 'true)
        (else (v-and (cdr exps))))) 

(define (v-or exps)
  (cond ((null? exps) 'false)
        ((not (false? (car exps))) 'true)
        (else (v-or (cdr exps))))) 

(define (range num)
  (define (range-helper start)
    (if (= start (- num 1))
        (cons start '()) 
        (cons start (range-helper (+ start 1)))))
  (range-helper 0)) 

(define (my-abs num)
  (if (< num 0)
      (* -1 num)
      num))

(define (v-even? num) (= (remainder num 2) 0)) 

(define (v-odd? num) (not (v-even? num))) 

(define (println data)
  (newline)
  (display data)
  (newline)
  'done) 

;TODO: Rename keywords to make fancier names
;; 1. cons -> pair
;; 2. car  -> head
;; 3. cdr  -> tail

