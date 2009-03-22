;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;;; Definição da linguagem EA: Expressão Aritmética
(define-type EA		; define-type é do PLAI-Scheme (selecione e tecle F1)
  [numero (n number?)]	; construtor de numero: recebe um número
  [adic (esq EA?)	; construtor de adição: recebe 2 EAs
       (dir EA?)]
  [subtr (esq EA?)	; construtor de subtração: recebe 2 EAs
       (dir EA?)])

;;; Avaliador: processa a sintaxe abstrata
(define (calcular uma-ea)
  (type-case EA uma-ea
             [numero (n) n] 	; se for num, retorna o valor
             [adic (e d) (+ (calcular e) (calcular d))] ; executa a soma
             [subtr (e d) (- (calcular e) (calcular d))] ; similar
))

;;; Parser em Scheme: converte sintaxe concreta em sintaxe abstrata
(define (analisar sexp)
  (cond
    [(number? sexp) (numero sexp)]	   ; se for um número, simplesmente use o construtor
    [(cons? sexp)			   ; se for um cons (lista)    
     (case (first sexp)			   ; olha o primeiro
       [(op+) 				   ; adição usando op+ em vez de + (ver Obs. abaixo)
        (adic (analisar (second sexp))     ; construtor e chamada recursiva
              (analisar (third sexp)))]
       [(op-) 				   ; subtração
        (subtr (analisar (second sexp))
               (analisar (third sexp)))]
       )
     ]
))
;;; Fim do parser

; Obs: usei op+ e op- no parser para ficar claro que estes são símbolos 
; da nossa sintaxe concreta, e não os operadores + e - nativos do Scheme.
; Note que os operadores + e - do Scheme são usados no avaliador para
; efetivamente realizar os cálculos aritméticos.


;;; Testes do analisador
(test (analisar '3) (numero 3))
(test (analisar '{op+ 3 4} ) (adic (numero 3) (numero 4)))
(test (analisar '{op+ 3 {op- 9 4}} ) (adic (numero 3) (subtr (numero 9) (numero 4))))

;;; Testes do avaliador
(test (calcular (analisar '3)) 3)
(test (calcular (analisar '{op+ 3 4})) 7)
(test (calcular (analisar '{op+ 3 {op- 9 4}})) 8)

;;; Testes para entender o define-type

(test (EA? (numero 3)) #t)
(test (numero? (numero 3)) #t)
(test (EA? (analisar '3)) #t)
(test (numero? (analisar '3)) #t)
(test (adic? (analisar '3)) #f)
(test (adic? (analisar '{+ 3 4})) #f) ; o + não é parte da sintaxe concreta!
(test (adic? (analisar '{op+ 3 4})) #t)
(test (adic-esq (analisar '{op+ 3 4})) (numero 3)) ; acesso a um campo do tipo