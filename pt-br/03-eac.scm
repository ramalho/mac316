;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;;; Definição da linguagem EAC: Expressão Aritmética Com substituição
(define-type EAC	
  [numero (n number?)]
  [adic (esq EAC?)
       (dir EAC?)]
  [subtr (esq EAC?)
       (dir EAC?)]
  [ident (nome symbol?)]
  [com (nome symbol?) (expr-nomeada EAC?) (corpo EAC?)]
  )

;; subst : \scheme|WAE| symbol \scheme|WAE| $\rightarrow$ \scheme|WAE|
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case EAC expr
             [numero (n) expr]
             [adic (e d) (adic (subst e sub-id val)
                             (subst d sub-id val))]
             [subtr (e d) (subtr (subst e sub-id val)
                             (subst d sub-id val))]
             [ident (v) (if (symbol=? v sub-id) val expr)]
             [com (ident-associado expr-nomeada corpo-associado)
                   (if (symbol=? ident-associado sub-id)
                       (com ident-associado
                             (subst expr-nomeada sub-id val)
                             corpo-associado)
                       (com ident-associado
                             (subst expr-nomeada sub-id val)
                             (subst corpo-associado sub-id val)))]
             )
  )



;;; Avaliador: processa a sintaxe abstrata
(define (calcular uma-eac)
  (type-case EAC uma-eac
             [numero (n) n] 	; se for num, retorna o valor
             [adic (e d) (+ (calcular e) (calcular d))] ; executa a soma
             [subtr (e d) (- (calcular e) (calcular d))] ; similar
             [ident (v) (error 'calcular "identificador livre")]
             [com (ident-associado expr-nomeada corpo-associado)
                   (calcular (subst corpo-associado
                                ident-associado
                                (numero (calcular expr-nomeada))))]

))

;;; Parser em Scheme: converte sintaxe concreta em sintaxe abstrata
(define (analisar sexp)
  (cond
    [(number? sexp) (numero sexp)]	   ; se for um número ou identificador, apenas...
    [(symbol? sexp) (ident sexp)]	   ; ... use o construtor correspondente
    [(cons? sexp)			   ; se for um cons (lista)    
     (case (first sexp)			   ; olha o primeiro
       [(op+) 				   ; adição usando op+ em vez de + (ver Obs. abaixo)
        (adic (analisar (second sexp))     ; construtor e chamada recursiva
              (analisar (third sexp)))]
       [(op-) 				   ; subtração
        (subtr (analisar (second sexp))
               (analisar (third sexp)))]
       [(seja) (com (first (second sexp))
                         (analisar (second (second sexp)))
                         (analisar (third sexp)))]
       )
     ]
))
;;; Fim do parser

; Obs: a operação "com" escreve-se como {seja ...} na sintaxe concreta.

;;; Testes do analisador
(test (analisar '3) (numero 3))
(test (analisar '{op+ 3 4} ) (adic (numero 3) (numero 4)))
(test (analisar '{op+ 3 {op- 9 4}} ) (adic (numero 3) (subtr (numero 9) (numero 4))))
(test (analisar '{seja (ident x) (op+ 1 2) (op+ 3 x)}) (com (ident 'x) (adic (numero 1) (numero 2) ) (adic (numero 3) (ident 'x))))
(begin
  (display (analisar '{seja (ident x) (op+ 1 2) (op+ 3 x)}))
  (newline)
  )
;;; Testes para entender o define-type

(test (EAC? (analisar '{seja x (op+ 1 2) (op+ 3 x)})) #t)
(test (numero? (analisar '3)) #t)
(test (adic? (analisar '3)) #f)
(test (adic? (analisar '{+ 3 4})) #f) ; o + não é parte da sintaxe concreta!
(test (adic? (analisar '{op+ 3 4})) #t)
(test (adic-esq (analisar '{op+ 3 4})) (numero 3)) ; acesso a um campo do tipo