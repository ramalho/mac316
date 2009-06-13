;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;; The Little Schemer
;; Chapter 5. Shadows

; pre-requisite
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; p.99
(define numeric?
  (lambda (s)
    (cond
      ((null? s) #f)
      ((and (atom? (car s)) (not (number? (car s)))) #f)
      ((or (number? (car s)) (numeric? (car s)))
       (cond
         ((or (eq? (car (cdr s)) '+)
              (eq? (car (cdr s)) '*)
              (eq? (car (cdr s)) '^)) (numeric? (cdr (cdr s))))
         (else #f))))))

;;; tests
(print-only-errors #t)

(test (numeric? '(3)) #t)
(test (numeric? '(3 + (4 * 5))) #t)
(test (numeric? '(3 3)) #f)

(include "test-summary.scm")