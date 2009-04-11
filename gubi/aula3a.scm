;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; definição de função
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])	; o que acontece se usarmos WAE aqui???

; expressão aceita pela linguagem
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
    (named-expr F1WAE?)
    (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)	; chamada de função
       (arg-expr F1WAE?)])	; melhor F1WAE ou WAE???


; tabela de símbolos
(define-type DefrdSub
  [mtSub] ; mt -> eMPty
  [aSub (name symbol?)					; nome
  		(value number?)				; valor
		(ds DefrdSub?)				; repositório
		])

; busca do símbolo
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
; interp : F1WAE list-of-FunDef DefrSub-> num
(define (interp a-wae defs ds)
  (type-case F1WAE a-wae
    [num (n) n]
    [add (l r) (+ (interp l defs ds) (interp r defs ds))]
    [sub (l r) (- (interp l defs ds) (interp r defs ds))]
    ; with: simplesmente adiciona na tabela de símbolos
    [with (bound-id named-expr body-expr)
      (interp body-expr defs 
              (aSub bound-id (interp named-expr defs ds) ds))]
    ; id: procura na tabela de símbolos
    [id (name) (lookup name ds)] 
    [app (fname arg-expr)
         (local [(define f (find-def fname defs))]
           (interp (fundef-body f) defs
                   (aSub (fundef-arg-name f)
                         (interp arg-expr defs ds)
                         ds) ;; Está certo isso??????????
                   ))]))

; find-def : sym list-of-FunDef -> FunDef
(define (find-def fname l)
  (cond
    [(empty? l) (error "no such function")]
    [else (if (symbol=? fname (fundef-fun-name (first l)))
              (first l)
              (find-def fname (rest l)))]))
; É verdade que find-def faz uma busca de complexidade exponencial???

(test/exn (lambda () (find-def 'x empty)) "no such function")
(test (find-def 'f (list
                    (fundef 'g 'y (num 12))
                    (fundef 'f 'y (num 10))))
      (fundef 'f 'y (num 10)))

; subst : F1WAE sym num -> F1WAE
(define (subst a-wae sub-id val)
  (type-case F1WAE a-wae
    [num (n) a-wae]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr body-expr)
      (with bound-id 
        (subst named-expr sub-id val)
        (if (symbol=? bound-id sub-id)	; qual a razão deste teste???
            body-expr
            (subst body-expr sub-id val)))]
    [id (name) (if (symbol=? name sub-id) ; e deste???
                   (num val)				
                   a-wae)]		; a-wae?? isso está correto? 
					; o que tem em a-wae nesse caso???
    [app (fname arg-expr)
         (app fname (subst arg-expr sub-id val))])) ; o que faz este subst???

(test (subst (add (num 1) (id 'x)) 'x 10)
      (add (num 1) (num 10)))
(test (subst (id 'x) 'x 10)
      (num 10))
(test (subst (id 'y) 'x 10)
      (id 'y))
(test (subst (sub (id 'x) (num 1)) 'y 10)
      (sub (id 'x) (num 1)))
(test (subst (app 'x (num 10)) 'y 12)
      (app 'x (num 10)))
(test (subst (app 'x (id 'y)) 'y 12)
      (app 'x (num 12)))
(test (subst (app 'y (num 10)) 'y 12)
      (app 'y (num 10)))


(test (subst (with 'y (num 17) (id 'x)) 'x 10)
      (with 'y (num 17) (num 10)))
(test (subst (with 'y (id 'x) (id 'y)) 'x 10)
      (with 'y (num 10) (id 'y)))
(test (subst (with 'x (id 'y) (id 'x)) 'x 10)
      (with 'x (id 'y) (id 'x)))

(test (interp (num 5) empty (mtSub))
      5)
(test (interp (add (num 1) (num 2)) empty (mtSub))
      3)
(test (interp (sub (num 1) (num 2)) empty (mtSub))
      -1)
(test (interp (with 'x (add (num 1) (num 17))
                (add (id 'x) (num 12)))
              empty (mtSub))
      30)
(test/exn (lambda ()
            (interp (id 'x) empty (mtSub)))
          "free variable")

(test/exn (lambda ()
            (interp (app 'f (num 10)) empty (mtSub)))
          "no such function")

(test (interp (app 'f (num 10))
              (list (fundef 'f
                            'y
                            (add (id 'y) (num 1)))) (mtSub))
      11)
(test (interp (app 'f (num 10))
              (list
               (fundef 'f
                       'y
                       (with 'y (num 7)
                             (id 'y)))) (mtSub))
      7)
