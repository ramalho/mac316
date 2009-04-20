;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")


; PLAI chapter 3, p. 16

(define-type WAE
  [%num (n number?)]
  [%add (lhs WAE?) (rhs WAE?)]
  [%sub (lhs WAE?) (rhs WAE?)]
  [%with (name symbol?) (named-expr WAE?) (body WAE?)]
  [%id (name symbol?)]
)

;; subst : WAE symbol WAE -> WAE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

; substitui sub-id por val em expr; a expressão resultante não tem
; instâncias livres de sub-id

; No capítulo 3 de PLAI há três versões para a função de substituição,
; aqui denominadas subst-v1, subst-v2 e subst-v3. A função é usada 
; apenas no interpretador (calc) e em diretamente em alguns testes. 
; Logo antes da definição de calc coloquei uma definição que associa 
; subst a sobst-v1, subst-v2 ou subst-v3, para facilitar os testes com 
; as três versões.

; PLAI section 3.1, p. 19-20
; subst renomeado para subst-v1
; esta implementação gera exceção nos testes t5, t6, t10 e t11 abaixo
(define (subst-v1 expr sub-id val)
  (type-case WAE expr
             [%num (n) expr]
             [%add (l r) (%add (subst-v1 l sub-id val)
                               (subst-v1 r sub-id val))]
             [%sub (l r) (%sub (subst-v1 l sub-id val)
                               (subst-v1 r sub-id val))]
             [%with (bound-id named-expr bound-body)
                    (if (symbol=? bound-id sub-id)
                        expr
                        (%with bound-id
                              named-expr
                              (subst-v1 bound-body sub-id val)))]

             [%id (v) (if (symbol=? v sub-id) val expr)]
  )
)

; PLAI section 3.3, p. 21-22 
; subst renomeado para subst-v2
; esta implementação gera exceção no teste t11
(define (subst-v2 expr sub-id val)
  (type-case WAE expr
             [%num (n) expr]
             [%add (l r) (%add (subst-v2 l sub-id val)
                               (subst-v2 r sub-id val))]
             [%sub (l r) (%sub (subst-v2 l sub-id val)
                               (subst-v2 r sub-id val))]
             [%with (bound-id named-expr bound-body)
                    (if (symbol=? bound-id sub-id)
                        expr
                        (%with bound-id
                               ; alteração: aplicar subst à expressão nomeada
                               (subst-v2 named-expr sub-id val) 
                               (subst-v2 bound-body sub-id val)))]

             [%id (v) (if (symbol=? v sub-id) val expr)]
  )
)

; PLAI section 3.3, p. 22 
; subst renomeado para subst-v3
; esta implementação passa em todos os testes de t1 a t11
(define (subst-v3 expr sub-id val)
  (type-case WAE expr
             [%num (n) expr]
             [%add (l r) (%add (subst-v3 l sub-id val)
                               (subst-v3 r sub-id val))]
             [%sub (l r) (%sub (subst-v3 l sub-id val)
                               (subst-v3 r sub-id val))]
             [%with (bound-id named-expr bound-body)
                    (if (symbol=? bound-id sub-id)
                        ; alteração: aplicar substituição à expressão nomeada
                        ; mas não ao corpo, quando o identificador usado é
                        ; o mesmo (veja exemplo no teste t11
                        (%with bound-id
                              (subst-v3 named-expr sub-id val)
                              bound-body)
                        (%with bound-id
                               (subst-v3 named-expr sub-id val) 
                               (subst-v3 bound-body sub-id val)))]

             [%id (v) (if (symbol=? v sub-id) val expr)]
  )
)

;; parse : sexp -> WAE
;; to convert s-expressions into WAEs

(define (parse sexp)
  (cond
   [(symbol? sexp) (%id sexp)]
   [(number? sexp) (%num sexp)]
   [(list? sexp)
    (case (first sexp)
      [(+) (%add (parse (second sexp))
                     (parse (third sexp)))]
      [(-) (%sub (parse (second sexp))
                     (parse (third sexp)))]
      [(with) (%with (first (second sexp))
                         (parse (second (second sexp)))
                         (parse (third sexp)))]
      )
    ]
   )
  )


;;; Definição da versão de substituição efetivamente usada no interpretador

; (define subst subst-v1) ; gera exceção nos testes t5, t6, t10 e t11 abaixo
; (define subst subst-v2) ; gera exceção no teste t11 abaixo
(define subst subst-v3) ; passa em todos os testes de t1 a t11

;; calc : WAE!number
;; evaluates WAE expressions by reducing them to numbers
(define (calc expr)
  (type-case WAE expr
             [%num (n) n]
             [%add (l r) (+ (calc l) (calc r))]
             [%sub (l r) (- (calc l) (calc r))]
             [%with (bound-id named-expr bound-body)
                   (calc (subst bound-body
                                bound-id
                                (%num (calc named-expr))))]
             [%id (v) (error 'calc "free identifier")]
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTES
 
; durante o desenvolvimento é melhor omitir os resultados "good", porque
; quando há muitos testes podemos não ver os "bad" no meio dos "good"
 
(print-only-errors #t)

;;; Testes para entender o define-type WAE

(test/pred (%id 'x) WAE?)
(test/pred (%with 'a (%num 3) (%id 'a)) WAE?)

; define-type também cria predicados para testar as variantes

(test/pred (%id 'x) %id?)
(test/pred (%with 'a (%num 3) (%id 'a)) %with?)

; e cria funções para acessar os campos de cada variante

(test (%id-name (%id 'x)) 'x) ; 'x é o valor do campo name na variante %id

; uma expresão WAE para usar nos testes a seguir
(define WITH_A (%with 'a (%num 3) (%id 'a)))

; campos da variante with:
(test (%with-name WITH_A) 'a)
(test (%with-named-expr WITH_A) (%num 3))
(test (%with-body WITH_A) (%id 'a))

;;; Testes para entender o type-case

(test (type-case WAE WITH_A
                 (%with (field1 field2 field3) field1)
                 (else (error "erro de sintaxe")))
      'a) ; 'a é o valor do campo field1 de %with

(test (type-case WAE WITH_A
                 (%with (field1 field2 field3) field2)
                 (else (error "erro de sintaxe")))
      (%num 3)) ; 'a é o valor do campo field2 de %with

;;; Testes de subst

(test (subst (%add (%num 5) (%id 'b)) 'b (%num 7))
      (%add (%num 5) (%num 7)))

(test (subst (%add (%num 5) (%id 'c)) 'c (%sub (%num 7) (%num 3)))
      (%add (%num 5) (%sub (%num 7) (%num 3))))

;;; Testes de calc

(test (calc WITH_A) 3)
(test (calc (%with 'x (%num 3) (%add (%num 17) (%id 'x)))) 20)
(test (calc (%with 'z (%add (%num 12) (%num 13)) (%add (%num 17) (%id 'z)))) 42)

;;; Testes de parse

(test (parse '{with {x {+ 5 5}} {+ x x}}) 
      (%with 'x (%add (%num 5) (%num 5)) (%add (%id 'x) (%id 'x))))


;;; Testes de parse e calc

; PLAI section 3.3, p. 21
(test (calc (parse '5)) 5) ; t1
(test (calc (parse '{+ 5 5})) 10) ; t2
(test (calc (parse '{with {x {+ 5 5}} {+ x x}} ) ) 20) ; t3
(test (calc (parse '{with {x 5} {+ x x}})) 10) ; t4
; t5: excecao "free identifier" com subst-v1
(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14) ; t5
; t6: excecao "free identifier" com subst-v1
(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4) ; t6
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15) ; t7
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8) ; t8
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10) ; t9
; t10: excecao "free identifier" com subst-v1
(test (calc (parse '{with {x 5} {with {y x} y}})) 5) ; t10
; t11: excecao "free identifier" com subst-v1 e subst-v2
(test (calc (parse '{with {x 5} {with {x x} x}})) 5) ; t11

; exibir contagem de falhas, exceções e testes
(define (contar-testes simbolo) 
  (length (filter (lambda (teste) (eq? simbolo (car teste))) 
                  plai-all-test-results)))

(display (list (contar-testes 'bad) "falhas," 
               (contar-testes 'exception) "excecoes em"
               (length plai-all-test-results) "testes"))
