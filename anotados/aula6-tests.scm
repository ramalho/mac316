;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(require "aula6.scm")
(print-only-errors #t)

;; Testes do Prof. Gubi:

; copiados de aula4a-tests.scm
(test (tonum (interp (parse '(with (k (fun (x) (+ 3 x))) (+ 4 (call k 2)))))) 9)

(test (tonum (interp (parse '{with {f {fun (x) (+ x 1)}} {f 2}}))) 3)
(test (parse '{with {x 3}
                {fun (y) {+ x y}}}) (%app (%fun 'x (%fun 'y (%add (%id 'x) (%id 'y)))) (%num 3)))


(test (tonum 
	   (interpd (parse 
                     '(with (k (fun (x) (+ 3 x))) (+ 4 (call k 2))))  
                    (mtSub)))
      9)

(test (parse 
       '{with {x 3} {fun (y) {+ x y}}}) 
      (%app (%fun 'x (%fun 'y (%add (%id 'x) (%id 'y)))) (%num 3)))


(test (tonum (interp (parse '{with {f {fun (x) (+ x 1)}} {f 2}}))) 3)

(test (parse '{with {x 3}
                {fun (y) {+ x y}}}) 
      (%app (%fun 'x (%fun 'y (%add (%id 'x) (%id 'y)))) (%num 3)))

(test
 (tonum
   (interp 
     (parse 
      '(with 
         (dq (with (d/dx (fun (f) 
                         (fun (x) 
                              (/(-(f (+ x 0.00001)) (f x)) 0.00001))))
                (d/dx (fun (x) (* x x))))) 
         (dq 9)))))
  18.0)

;; Nossos testes

(test (tonum (interp (parse '{with {a 5}
                                   {with {f {fun (x) (+ x a)}} {f 7}}}))) 12)

;; O que está acontecendo aqui é o seguinte: quando o s1 é 
;; aplicado em (s1 3) o símbolo "f" é encontrado na closure
;; (veja este teste levantar uma exceção em aula4-tests.scm, pois
;; lá a linguagem não implementa closures)

(test
 (interp 
  (parse 
   '(with 
     (s1 (with (s2 (fun (f) 
                        (fun (x) 
                             (f (+ x 5)))))
               (s2 (fun (x) (+ x 2))))) 
     (s1 3))))
 (numV 10))


(include "test-summary.scm")
