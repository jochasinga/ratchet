#lang racket

(provide atom?
         ∈?
         in?
         lat?)

(define atom?
  (lambda (a)
    (and (not (null? a))
         (not (pair? a)))))
    
(define ∈?
  (lambda (s l) (in? s l)))

(define in?
  (lambda (s l)
    (cond
      [(null? l) #f]
      [(eq? s (car l)) #t]
      [(atom? (car l)) (in? s (cdr l))]
      [else #f])))


(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))
       
  
