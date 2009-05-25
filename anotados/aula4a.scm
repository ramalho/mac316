;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;;#reader(lib "reader.ss" "plai" "lang")


(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE?) (ds DefrdSub?)]
)

; expressão aceita pela linguagem
(define-type FAE
  [%num  (n number?)]
  [%add  (lhs FAE?) (rhs FAE?)]
  [%sub  (lhs FAE?) (rhs FAE?)]
  [%fun  (param symbol?)  (body FAE?)]
  [%id   (name symbol?)]
  [%app  (fun-expr FAE?) (arg-expr FAE?)])

; busca linear
(define (lookup name ds)
  (type-case DefrdSub ds
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-ds)
                   (if (symbol=? bound-name name)
                       bound-value		; valor associado
                       (lookup name rest-ds) ; procura no resto
                       )
                   ]
             )
  )

; interp : FAE -> FAE
(define (interp expr) (interpd expr [mtSub]))

; interpd : FAE ds -> FAE
(define (interpd expr ds )
  (type-case FAE expr
    [%num (n) expr]  ; agora pode retornar as duas coisas, é bom deixar o "tipo"
    [%add (l r) (add-numbers (interpd l ds) (interpd r ds))]
    [%sub (l r) (sub-numbers (interpd l ds) (interpd r ds))]
    [%id (name) (lookup name ds)]
    [%fun (arg body) expr]
    [%app (fun-expr arg-expr)
         (local [(define f (interpd fun-expr ds))]
           (interpd (%fun-body f) (aSub (%fun-param f) (interpd arg-expr ds) ds))
           )
         ]
    ))

(define (tonum nn) 
  (type-case FAE nn
             [%num (n) n]
             [else (error "Bad num")]
             ))
                    
(define (add-numbers l r) 
    (%num (+ (tonum l) (tonum r))))

(define (sub-numbers l r) 
    (%num (- (tonum l) (tonum r))))



; Rotina auxiliar

;(load "Parse-FAE.scm")
; Rotina auxiliar
(define (read-prog arq) (call-with-input-file arq (lambda (a) (read a))))

; Parser
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
	   [(with) (%app (%fun (first (second sexp)) ; param
						 (parse (third sexp))) ;body
					(parse (second (second sexp))) ;valor
					)
		]
	   [(fun) (%fun (first (second sexp)) (parse (third sexp)))]
	   [(call) (%app (parse (second sexp)) (parse (third sexp)))]
	   [else (%app (parse (first sexp)) (parse (second sexp)))]
	   )
	 ]
))


;;(include "aula4a-tests.scm")
