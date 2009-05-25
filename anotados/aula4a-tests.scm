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

(test (tonum (interp (parse '{with {a 5}
                                   {with {f {fun (x) (+ x a)}} {f 7}}}))) 12)

;; O que está acontecendo aqui é o seguinte:
;; quando o (with (s2 (fun(f)... recebe de volta o corpo de fun(f)
;; para associar ao nome "s2", o símbolo "f" não está imediatamente
;; disponível para substituição, mas está contido na definição de fun(x)
;; (por isso, o erro depende de funções aninhadas, esó se manifesta quando
;; você força uma chamada de fun(x) (com "(s1 3)", por exemplo).
(test/exn
 (interp 
  (parse 
   '(with 
     (s1 (with (s2 (fun (f) 
                        (fun (x) 
                             (f (+ x 5)))))
               (s2 (fun (x) (+ x 2))))) 
     (s1 3))))
 "lookup: no binding for identifier: f")


(include "test-summary.scm")
