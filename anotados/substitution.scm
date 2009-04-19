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

; PLAI section 3.1, p. 19-20
(define (subst expr sub-id val)
  (type-case WAE expr
             [%num (n) expr]
             [%add (l r) (%add (subst l sub-id val)
                               (subst r sub-id val))]
             [%sub (l r) (%sub (subst l sub-id val)
                               (subst r sub-id val))]
             [%with (bound-id named-expr bound-body)
                    (if (symbol=? bound-id sub-id)
                        expr
                        (%with bound-id
                              named-expr
                              (subst bound-body sub-id val)))]

             [%id (v) (if (symbol=? v sub-id) val expr)]
  )
)


;; parse : sexp $\longrightarrow$ WAE
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

; exibir contagem de falhas e testes
(display 
  (list 
    (length (filter (lambda (l) (eq? 'bad (car l))) plai-all-test-results)) 
    "falhas em"
    (length plai-all-test-results) "testes")) 
