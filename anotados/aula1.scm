;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")

;;; Sintaxe abstrata AE (Arithmetic Expression)
;;; para facilitar a leitura do código, os identificadores definidos
;;; na linguagem abstrata usam o prefixo %

(define-type AE		; define-type é do PLAI-Scheme (selecione e tecle F1)
  [%num (n number?)]	; construtor 'num' e recebe um número
  [%add (lhs AE?)	; construtor 'add' recebe 2 AEs, lhs (left) e rhs (right)
       (rhs AE?)]
  [%sub (lhs AE?)	; similar.
       (rhs AE?)])

;;; Parser em Scheme
(define (parse sexp)
  (cond
   [(number? sexp) (%num sexp)]	        ; se for um número, apenas use o construtor
   [(cons? sexp)			; se for um cons (lista)
    
    (case (first sexp)			; examina o primeiro elemento da lista
      [(+)  		                ; este + é da nossa sintaxe concreta
	   (%add (parse (second sexp))  ; construtor e chamada recursiva
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
;;; End


;; Avaliador
 (define (calc an-ae)
         (type-case AE an-ae
                [%num (n) n] 	; se for num, retorna o valor
                [%add (l r) (+ (calc l) (calc r))] ; add -> faz a soma
                [%sub (l r) (- (calc l) (calc r))] ; similar
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTES
 
; durante o desenvolvimento é melhor omitir os resultados "good", porque
; quando há muitos testes podemos não ver os "bad" no meio dos "good"
 
(print-only-errors #t)
 
;;; Testes para entender o define-type

; o define-type cria um predicado para testar expressões completas

(test (AE? (%num 3)) #t)
(test/pred (%num 3) AE?) ; outra forma de escrever o teste acima, com menos ()
(test/pred (%add (%num 3) (%num 4)) AE?)

; define-type também cria predicados para testar as variantes

(test/pred (%num 3) %num?)
(test/pred (%sub (%num 7) (%num 4)) %sub?)
(test/pred (%add (%num 5) (%sub (%num 7) (%num 4))) %add?)

; e cria funções para acessar os campos de cada variante

(test (%num-n (%num 7)) 7) ; 7 é o valor do campo n na variante %num
(test (%add-rhs (%add (%num 2) (%num 5))) (%num 5)) ; campo rhs de %add
(test (%num-n (%add-lhs (%add (%num 2) (%num 5)))) 2)

;;; Testes do parser

(define NINE_ABS (%num 9))
(define ADD_ABS (%add (%num 3) (%num 4)))
(define SUB_ABS (%sub (%num 3) (%add (%num 5) (%num 2))))

(test (parse '9) NINE_ABS)
(test (parse '{+ 3 4}) ADD_ABS)
(test (parse '{- 3 {+ 5 2}}) SUB_ABS)

(test/pred (parse '3) AE?)
(test (%add? (parse '3)) #f) ; '3 não é uma adição
(test/pred (parse '{+ 3 4}) AE?)
(test/pred (parse '{+ 3 4}) %add?) ; isto é uma adição

; este parser devolve #<void> quando a sintaxe é inválida 
(test/pred (parse '{add 3 4}) void?) ; add não é parte da sintaxe concreta!
(test/pred (parse '{%add 3 4}) void?) ; %add também não é da sintaxe concreta!
(test (%add? (parse '{+ 3 4})) #t)
(test (%add-lhs (parse '{+ 3 4})) (%num 3)) ; acesso a um campo do tipo 

; (test (numero? (parse '3)) #t)

;;; contagem de falhas e testes
(display 
  (list 
    (length (filter (lambda (l) (eq? 'bad (car l))) plai-all-test-results)) "falhas em"
    (length plai-all-test-results) "testes")) 

