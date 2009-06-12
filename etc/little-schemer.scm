;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

; p.44
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (car l))
             (firsts (cdr l)))))))

; my sidetrip
(define append
  (lambda (a l)
    (cond
      ((null? l) (cons a '()))
      (else
       (cons (car l) (append a (cdr l)))))))
         
(define last
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (car l))
      (else
       (last (cdr l))))))

(define butlast
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) '())
      (else
       (cons (car l) (butlast (cdr l)))))))

(define reverse
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (last l) (reverse (butlast l)))))))

; end of sidetrip

; p.48

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

; p.51

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))

; p.52

(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (list old1 old2)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))

; p.53
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else
       (cons (car lat) (multirember a (cdr lat)))))))

; p.56
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertR new old (cdr lat)))))))

; p.57
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat) (multisubst new old (cdr lat)))))))


;;; tests
(print-only-errors #t)
(test (subst 'x 'a '(a b c)) '(x b c))
(test (subst 'x 'b '(a b c)) '(a x c))
(test (subst 'x 'c '(a b c)) '(a b x))
(test (subst 'x 'z '(a b c)) '(a b c))

(test (subst2 'x 'a 'b '(a b c)) '(x b c))
(test (subst2 'x 'b 'a '(a b c)) '(x b c))
(test (subst2 'x 'b 'c '(a b c)) '(a x c))
(test (subst2 'x 'c 'd '(a b c)) '(a b x))
(test (subst2 'x 'z 'a '(a b c)) '(x b c))
(test (subst2 'x 'z 'y '(a b c)) '(a b c))

(test (multirember 'a '(a b c)) '(b c))
(test (multirember 'a '(a a c)) '(c))
(test (multirember 'a '(a b a)) '(b))
(test (multirember 'a '(a a a)) '())

(test (multiinsertR 'x 'a '(a b c)) '(a x b c))
(test (multiinsertR 'x 'a '(a a c)) '(a x a x c))
(test (multiinsertR 'x 'a '(a a a)) '(a x a x a x))
(test (multiinsertR 'x 'a '(b c)) '(b c))

(test (multiinsertL 'x 'a '(a b c)) '(x a b c))
(test (multiinsertL 'x 'a '(a a c)) '(x a x a c))
(test (multiinsertL 'x 'a '(a a a)) '(x a x a x a))
(test (multiinsertL 'x 'a '(b c)) '(b c))

(test (multisubst 'x 'a '(a b c)) '(x b c))
(test (multisubst 'x 'a '(a a c)) '(x x c))
(test (multisubst 'x 'a '(a a a)) '(x x x))
(test (multisubst 'x 'b '(a b b)) '(a x x))
(test (multisubst 'x 'c '(c b c)) '(x b x))
(test (multisubst 'x 'z '(a b c)) '(a b c))


(include "test-summary.scm")