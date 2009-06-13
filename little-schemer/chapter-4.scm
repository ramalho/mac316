;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;; The Little Schemer
;; Chapter 4. Numbers Games

; pre-requisite
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; p.59
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))
  
; p.60
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (o+ (add1 n) (sub1 m))))))

; p.61
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (o- (sub1 n) (sub1 m))))))

; p.64
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup) (addtup (cdr tup)))))))

; p.65
(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (o+ n (o* n (sub1 m)))))))

; p.69
(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else
       (cons (o+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

; p.72
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (o> (sub1 n) (sub1 m))))))

; p.73
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (o< (sub1 n) (sub1 m))))))

; p.74
(define o=
  (lambda (n m)
    (and (not (o< n m)) (not (o> n m)))))

(define o^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else
       (o* a (o^ a (sub1 b)))))))


; p.76
(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (len (cdr lat)))))))

(define pick0
  (lambda (n lat)
    (cond
      ((zero? n) (car lat))
      (else
       (pick0 (sub1 n) (cdr lat))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

; p.77
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons (car lat) (no-nums (cdr lat)))))))

; p.78
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (o= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
      (else
       (occur a (cdr lat))))))
; p.79

(define one?
  (lambda (n)
    (and (number? n) (zero? (sub1 n)))))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;; tests
(print-only-errors #t)

(test (addtup '(3)) 3)
(test (addtup '(1 2 3 4)) 10)
(test (addtup '(0)) 0)

(test (o* 6 7) 42)
(test (o* 11 11) 121)

(test (tup+ '() '()) '())
(test (tup+ '(1) '(9)) '(10))
(test (tup+ '(1 2) '(3 4)) '(4 6))
(test (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
(test (tup+ '(1 2 3) '(1 2)) '(2 4 3))
(test (tup+ '(1 2) '(1 2 3)) '(2 4 3))

(test (o> 2 3) #f)
(test (o> 3 2) #t)
(test (o> 2 2) #f)

(test (o< 2 3) #t)
(test (o< 3 2) #f)
(test (o< 2 2) #f)

(test (o= 2 3) #f)
(test (o= 3 2) #f)
(test (o= 2 2) #t)

(test (o^ 2 3) 8)
(test (o^ 3 3) 27)
(test (o^ 2 1) 2)
(test (o^ 2 0) 1)

(test (len '()) 0)
(test (len '(1)) 1)
(test (len '(1 2)) 2)

(test (pick0 0 '(a b c)) 'a)
(test (pick0 1 '(a b c)) 'b)
(test (pick0 2 '(a b c)) 'c)

(test (pick 1 '(a b c)) 'a)
(test (pick 2 '(a b c)) 'b)
(test (pick 3 '(a b c)) 'c)

(test (rempick 1 '(a b c)) '(b c))
(test (rempick 2 '(a b c)) '(a c))
(test (rempick 3 '(a b c)) '(a b))

(test (no-nums '(a 1 b)) '(a b))
(test (no-nums '(1 a b 2)) '(a b))
(test (no-nums '(1 2)) '())

(test (all-nums '(a 1 b)) '(1))
(test (all-nums '(1 a b 2)) '(1 2))
(test (all-nums '(a b)) '())

(test (eqan? 1 1) #t)
(test (eqan? 1 2) #f)
(test (eqan? 'a 'a) #t)
(test (eqan? 'a 'b) #f)
(test (eqan? 1 'a) #f)

(test (occur 2 '(1 2 3)) 1)
(test (occur 2 '(1 3 5)) 0)
(test (occur 2 '(2 2 2)) 3)

(test (occur 'a '(a 2 3)) 1)
(test (occur 'a '(1 3 5)) 0)
(test (occur 'a '(a a a)) 3)

(test (one? 1) #t)
(test (one? '1) #t)
(test (one? 2) #f)
(test (one? 0) #f)
(test (one? 'a) #f)

(test (rempick2 1 '(a b c)) '(b c))
(test (rempick2 2 '(a b c)) '(a c))
(test (rempick2 3 '(a b c)) '(a b))

(include "test-summary.scm")