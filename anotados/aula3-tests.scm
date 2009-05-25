;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;; #reader(lib "reader.ss" "plai" "lang")

(include "aula3.scm")

;LR| durante o desenvolvimento é melhor omitir os resultados "good", porque
;LR| quando há muitos testes podemos não ver os "bad" no meio dos "good"
 
(print-only-errors #t)

;LR| O teste comentado abaixo está no código do Prof. Gubi, mas resulta 'bad
;LR| porque no corpo do lambda o find-def não é invocado (só seria se a função
;LR| definida no lambda fosse invocada), então a exceção esperada não ocorre

;(test/exn (lambda () (find-def 'x empty)) "no such function")

;LR| creio que esta é intenção do teste acima:
(test/exn (find-def 'x empty) "no such function")


(test (find-def 'f (list
                    ($fundef 'g 'y (%num 12))
                    ($fundef 'f 'y (%num 10))))
      ($fundef 'f 'y (%num 10)))


(test (subst (%add (%num 1) (%id 'x)) 'x 10)
      (%add (%num 1) (%num 10)))
(test (subst (%id 'x) 'x 10)
      (%num 10))
(test (subst (%id 'y) 'x 10)
      (%id 'y))
(test (subst (%sub (%id 'x) (%num 1)) 'y 10)
      (%sub (%id 'x) (%num 1)))
(test (subst (%app 'x (%num 10)) 'y 12)
      (%app 'x (%num 10)))
(test (subst (%app 'x (%id 'y)) 'y 12)
      (%app 'x (%num 12)))
(test (subst (%app 'y (%num 10)) 'y 12)
      (%app 'y (%num 10)))


(test (subst (%with 'y (%num 17) (%id 'x)) 'x 10)
      (%with 'y (%num 17) (%num 10)))
(test (subst (%with 'y (%id 'x) (%id 'y)) 'x 10)
      (%with 'y (%num 10) (%id 'y)))
(test (subst (%with 'x (%id 'y) (%id 'x)) 'x 10)
      (%with 'x (%id 'y) (%id 'x)))

(test (interp (%num  5) empty)
      5)
(test (interp (%add (%num  1) (%num  2)) empty)
      3)
(test (interp (%sub (%num  1) (%num  2)) empty)
      -1)
;XXX
(test (interp (%with 'x (%add (%num  1) (%num  17))
                (%add (%id 'x) (%num  12)))
              empty)
      30)

;LR| Aqui o Prof. Gubi colocou a chamada ao interp dentro de um lambda, e o
;LR| resultado é que este teste resulta 'bad porque a exceção não ocorre
;(test/exn (lambda ()
;            (interp (%id 'x) empty))
;          "free variable")

;LR| este teste exercita a exceção
(test/exn (interp (%id 'x) empty)
          "free variable")

;LR| Novamente a questão do lambda...
;(test/exn (lambda ()
;            (interp (%app  'f (%num  10)) empty))
;          "no such function")

;LR| este teste exercita a exceção
(test/exn (interp (%app  'f (%num  10)) empty)
          "no such function")

(test (interp (%app  'f (%num  10))
              (list ($fundef 'f
                            'y
                            (%add (%id 'y) (%num  1)))))
      11)
(test (interp (%app  'f (%num  10))
              (list
               ($fundef 'f
                       'y
                       (%with 'y (%num  7)
                             (%id 'y)))))
      7)

(include "test-summary.scm")
