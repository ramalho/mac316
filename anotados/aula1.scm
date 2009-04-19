;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define-type AE					; tipo
  [num (n number?)]				; construtor 'num' e recebe um n?mero
  [add (lhs AE?)				; construtor 'add' recebe 2 AE s
       (rhs AE?)]
  [sub (lhs AE?)				; similar.
       (rhs AE?)])

;;; Parser em Scheme
(define (parse sexp)
  (cond
   [(number? sexp) (num sexp)]	           ; se for um número, simplesmente use o construtor
   [(cons? sexp)			   ; se for um cons (lista)
    
    (case (first sexp)			   ; olha o primeiro
      [(+) 				   ; soma
	   (add (parse (second sexp))      ; construtor e chamada recursiva
                (parse (third sexp)))			
      ]
		  
      [(-) 				  ; subtração
  	   (sub (parse (second sexp))
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
                [num (n) n] 	; se for num, retorna o valor
                [add (l r) (+ (calc l) (calc r))] ; add -> faz a soma
                [sub (l r) (- (calc l) (calc r))] ; similar
  ))

