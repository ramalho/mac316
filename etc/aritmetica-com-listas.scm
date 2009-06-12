;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define zero '())

(define (incr n)
  (cons #t n))

(define um (incr zero))
(define dois (incr um))
(define tres (incr dois))
(define cinco (incr (incr tres)))

(define (decr n)
  (cdr n))

(define (+ a b)
  (if (null? b)
      a
      (+ (incr a) (decr b))))

(define (- a b)
  (if (null? b) 
      a
      (- (decr a) (decr b))))

(define (* a b)
  (if (null? b)
      '()
      (+ a (* a (decr b)))))

(define (^ a b)
  (if (null? b)
      um
      (* a (^ a (decr b)))))

(define (exibir expr val)
  (display expr)
  (display " = ")
  (display (length val))
  (newline)
)

(exibir '(+ dois tres) (+ dois tres))
(exibir '(- cinco tres) (- cinco tres))
(exibir '(* tres dois) (* tres dois))
(exibir '(* cinco (* cinco (* cinco cinco))) (* cinco (* cinco (* cinco cinco))))
(exibir '(^ tres dois) (^ tres dois))
(exibir '(^ dois cinco) (^ dois cinco))
(exibir '(^ cinco um) (^ cinco um))