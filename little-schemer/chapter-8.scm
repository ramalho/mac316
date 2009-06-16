;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
;; The Little Schemer
;; Chapter 8. Lambda the Ultimate

; p.126
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else
       (cons (car l) (rember-f test? a (cdr l)))))))

; my sidetrip: currying

(define make-adder
  (lambda (increment)
    (lambda (n)
      (+ n increment))))

(define adder10 (make-adder 10))

; p.129
(define mk-rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else
         (cons (car l) ((mk-rember-f test?) a (cdr l))))))))
                   
; p.130
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons new lat))
        (else
         (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons old (cons new (cdr lat))))
        (else
         (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

;;; tests
(print-only-errors #t)

(test (rember-f equal? 3 '(1 2 3 4 5)) '(1 2 4 5))
(test ((make-adder 5) 3) 8)
(test (adder10 3) 13)
(test ((mk-rember-f equal?) 3 '(1 2 3 4 5)) '(1 2 4 5))
(test ((insertL-f eq?) 'x 'b '(a b c)) '(a x b c))
(test ((insertR-f eq?) 'x 'b '(a b c)) '(a b x c))

(include "test-summary.scm")