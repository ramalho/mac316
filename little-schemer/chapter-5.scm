;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
;; The Little Schemer
;; Chapter 5. *Oh My Gawd*: It's Full of Stars

; pre-requisite
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; p.81
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((eq? (car l) a) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l)))))))

; p.82
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
      ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
      (else (cons (car l) (insertR* new old (cdr l)))))))

; p.84
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((list? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
      ((eq? (car l) a) (add1 (occur* a (cdr l))))
      (else
       (occur* a (cdr l))))))

; p.85
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
      ((eq? (car l) old) (cons new (subst* new old (cdr l))))
      (else
       (cons (car l) (subst* new old (cdr l)))))))

; p.86
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
      ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
      (else
       (cons (car l) (insertL* new old (cdr l)))))))

; p.87
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((list? (car l)) (or (member* a (car l)) (member* a (cdr l))))
      ((eq? (car l) a) #t)
      (else
       (member* a (cdr l))))))

; p.88
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; p.91

; from Chapter 4, p.71
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b)) ; changed o= to =
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

(define eqlist?
  (lambda (a b)
  (cond
    ((and (empty? a) (empty? b)) #t)
    ((not (empty? a))
     (cond
       ((empty? b) #f)
       ((and (list? (car a)) (list? (car b))) (and (eqlist? (car a) (car b)) (eqlist? (cdr a) (cdr b))))
       ((not (list? (car a)))
        (cond
          ((list? (car b)) #f)
          (else
           (and (eqan? (car a) (car b)) (eqlist? (cdr a) (cdr b))))))
       (else #f)))
     (else #f))))
     

;;; tests
(print-only-errors #t)

(test (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick))))
(test (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) '(((tomato)) ((bean)) (and ((flying)))))
(test (rember* 1 '(1 2 3)) '(2 3))
(test (rember* 9 '(1 2 3)) '(1 2 3))
(test (rember* 1 '((1 2) 3)) '((2) 3))
(test (rember* 1 '(((1) 2) 3)) '((() 2) 3))

(test (insertR* 'roast 'chuck 
                '((how much (wood))
                  could
                  ((a (wood) chuck))
                  (((chuck)))
                  (if (a) ((wood chuck)))
                  could chuck wood))
      '((how much (wood)) 
        could 
        ((a (wood) chuck roast))
        (((chuck roast)))
        (if (a) ((wood chuck roast)))
        could chuck roast wood))

(test (occur* 'x '(x y z)) 1)

(test (occur* 'wood '((how much (wood)) could
                ((a (wood) chuck))
                (((chuck)))
                (if (a) ((wood chuck)))
                could chuck wood)) 4)

(test (subst* 'spam 'wood 
              '((how much (wood)) 
                could
                ((a (wood) chuck))
                (((chuck)))
                (if (a) ((wood chuck)))
                could chuck wood))
      '((how much (spam))
        could
        ((a (spam) chuck))
        (((chuck)))
        (if (a) ((spam chuck)))
        could chuck spam))

(test (insertL* 'x 'a '(a b)) '(x a b))

(test (insertL* 'pecker 'chuck 
                '((how much (wood))
                  could
                  ((a (wood) chuck))
                  (((chuck)))
                  (if (a) ((wood chuck)))
                  could chuck wood))
      '((how much (wood))
        could
        ((a (wood) pecker chuck))
        (((pecker chuck)))
        (if (a) ((wood pecker chuck)))
        could pecker chuck wood))

(test (member* 'chips '((potato) (chips ((with) fish) (chips)))) #t)

(test (leftmost '((potato) (chips ((with) fish) (chips)))) 'potato)

(test (eqlist? '(1 2 3) '(1 2 3)) #t)
(test (eqlist? '((1) 2 3) '((1) 2 3)) #t)
(test (eqlist? '((1) 2 3) '(1 2 3)) #f)
(test (eqlist? '() '(1)) #f)
(test (eqlist? '(1) '()) #f)
(include "test-summary.scm")