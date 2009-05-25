;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(include "aula4a.scm")
(print-only-errors #t)

;; Testes do Prof. Gubi:

(test (tonum (interp (parse '(with (k (fun (x) (+ 3 x))) (+ 4 (call k 2)))))) 9)

(test (tonum (interp (parse '{with {f {fun (x) (+ x 1)}} {f 2}}))) 3)
(test (parse '{with {x 3}
                {fun (y) {+ x y}}}) (%app (%fun 'x (%fun 'y (%add (%id 'x) (%id 'y)))) (%num 3)))

;; Nossos testes


(include "test-summary.scm")
