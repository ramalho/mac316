;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define-syntax define-cps
  (syntax-rules ()
    [(define-cps (f arg) body)
     (define-cps f (lambda (arg) body))]
    [(define-cps v val)
     (define v ((cps val) (lambda (x) x)))]))
(define-syntax cps
  (syntax-rules (+ lambda web-read)
    [(cps (+ e1 e2))
     (lambda (k)
        ((cps e1) (lambda (l-val)
                       ((cps e2) (lambda (r-val)
                                    (k (+ l-val r-val)))))))]
    [(cps (lambda (a) body))
     (lambda (k)
        (k (lambda (a dyn-k)
              ((cps body) dyn-k))))]
    [(cps (web-read prompt))
     (lambda (k)
        (web-read/k prompt k))]
    [(cps (f a))
     (lambda (k)
        ((cps f ) (lambda (f-val)
                     ((cps a) (lambda (a-val)
                                 (f-val a-val k))))))]
    [(cps v)
     (lambda (k) (k v))]))
(define-syntax run
  (syntax-rules ()
    [(run e) ((cps e)
                (lambda (x)
                   (error "terminating with value" x)))]))
