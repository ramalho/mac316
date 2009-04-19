;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")

;;; Sintaxe abstrata AE (Arithmetic Expression)
;;; para facilitar a leitura do código, os identificadores definidos
;;; na linguagem abstrata usam o prefixo %

; PLAI section 1.2, p. 6
(define-type AE	     ; define-type é do PLAI-Scheme (selecione e tecle F1)
  [%num (n number?)] ; construtor '%num' e recebe um número
  [%add (lhs AE?)    ; construtor '%add' recebe 2 AEs: lhs (left), rhs (right)
        (rhs AE?)]
  [%sub (lhs AE?)    ; similar.
        (rhs AE?)])

;;; Parser em Scheme

; PLAI section 1.3, p.8
(define (parse sexp)
  (cond
   [(number? sexp) (%num sexp)]	      ; sexp é número, apenas use o construtor
   [(list? sexp)		      ; se sexp é lista...    
     (case (first sexp)		      ; ...examinar o primeiro item da lista
       [(+)  		              ; este + é da nossa sintaxe concreta
	   (%add (parse (second sexp)); construtor e chamada recursiva
                 (parse (third sexp)))			
       ]
		  
       [(-) 				; subtração
   	   (%sub (parse (second sexp))
                 (parse (third sexp)))
      ]
     )
    ]
   )
  )

;; Avaliador (interpretador de AE)
;; calc : AE -> number
;; consome uma AE e computa o número correspondente

; PLAI chapter 2, p. 13
(define (calc an-ae)
  (type-case AE an-ae
             [%num (n) n] 	                ; se for num, devolve o valor
             [%add (l r) (+ (calc l) (calc r))] ; somar usando + do Scheme
             [%sub (l r) (- (calc l) (calc r))] ; similar
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTES
 
; durante o desenvolvimento é melhor omitir os resultados "good", porque
; quando há muitos testes podemos não ver os "bad" no meio dos "good"
 
(print-only-errors #t)

;;; Testes para entender o define-type

; o define-type cria um predicado para testar expressões completas

(test (AE? (%num 3)) #t) ; %num é o construtor de números
(test/pred (%num 3) AE?) ; outra forma de escrever o teste acima, com menos ()
(test/pred (%add (%num 3) (%num 4)) AE?) ; %add é o construtor de adições

; define-type também cria predicados para testar as variantes

(test/pred (%num 3) %num?) ; %num? testa se uma expressão é %num
(test/pred (%sub (%num 7) (%num 4)) %sub?)
(test/pred (%add (%num 5) (%sub (%num 7) (%num 4))) %add?)

; e cria funções para acessar os campos de cada variante

(test (%num-n (%num 7)) 7) ; 7 é o valor do campo n na variante %num
(test (%add-rhs (%add (%num 2) (%num 5))) (%num 5)) ; campo rhs de %add
(test (%num-n (%add-lhs (%add (%num 2) (%num 5)))) 2)

;;; Testes do parser

(test (parse '9) 
      (%num 9))
(test (parse '{+ 3 4}) 
      (%add (%num 3) (%num 4)))
(test (parse '{- 3 {+ 5 2}}) 
      (%sub (%num 3) (%add (%num 5) (%num 2))))

(test/pred (parse '3) AE?)
(test (%add? (parse '3)) #f) ; '3 não é uma adição
(test/pred (parse '{+ 3 4}) AE?)
(test/pred (parse '{+ 3 4}) %add?) ; isto é uma adição

; este parser devolve #<void> quando a sintaxe é inválida 
(test/pred (parse '{add 3 4}) void?) ; add não é parte da sintaxe concreta
(test/pred (parse '{%add 3 4}) void?) ; %add também não é da sintaxe concreta
(test/pred (parse '{+ 3 4}) AE?) ; esta é a nossa sintaxe concreta
(test (%add-lhs (parse '{+ 3 4})) (%num 3)) ; acessar campo lhs da variante add

;;; Testes para entender o type-case

(test (type-case AE (%num 3)
                 (%num (x) x)
                 (else (error "erro de sintaxe")))
      3) ; nesta AE, 3 é o valor do campo x de %num

(test (type-case AE (%add (%num 3) (%num 4))
                 (%num (x) x)
                 (%add (l r) r)
                 (else (error "erro de sintaxe")))
      (%num 4)) ; (%num 4) é o valor do campo r de %add

(test/exn (type-case AE (%add (%num 3) (%num 4))
                     (%num (x) x)
                     (else (error "erro de sintaxe")))
          "erro de sintaxe") ; neste type-case a variante %add não existe 

;;; Testes do interpretador

; PLAI chapter 2, p. 13
(test (calc (parse '3)) 3)
(test (calc (parse '{+ 3 4})) 7)
(test (calc (parse '{+ {- 3 4} 7})) 6)

; exibir contagem de falhas, exceções e testes
(define (contar-testes simbolo) 
  (length (filter (lambda (teste) (eq? simbolo (car teste))) 
                  plai-all-test-results)))

(display (list (contar-testes 'bad) "falhas," 
               (contar-testes 'exception) "excecoes em"
               (length plai-all-test-results) "testes"))
