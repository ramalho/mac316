;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;; The Little Schemer
;; Chapter 5. *Oh My Gawd*: It's Full of Stars

; pre-requisite
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((eq? (car l) a) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
      ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
      (else (cons (car l) (insertR* new old (cdr l)))))))

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

(include "test-summary.scm")