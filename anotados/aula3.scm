;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; Neste arquivo o Prof. Gubi colocou vários comentários e perguntas.

; Os comentários que indicam páginas no livro PLAI e aqueles marcados 
; com ;LR| são meus (Luciano Ramalho)


; definição de função
; PLAI section 4.1, p. 28
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


; interp : F1WAE list-of-FunDef -> num
(define (interp a-wae defs)
  (type-case F1WAE a-wae
    [num (n) n]
    [add (l r) (+ (interp l defs) (interp r defs))]
    [sub (l r) (- (interp l defs) (interp r defs))]
    [with (bound-id named-expr body-expr)
      (interp (subst body-expr
                     bound-id
                     (interp named-expr defs))
              defs)]
    [id (name) (error 'interp "free variable")] ; por que o aviso de erro???
    [app (fname arg-expr)
         (local 
             [(define f (find-def fname defs))]
           (interp (subst 
                    (fundef-body f)          ; corpo de f
                    (fundef-arg-name f)      ; argumento formal de f
                    (interp arg-expr defs)   ; valor do argumento
                    )
                   defs))]
    )
  )

; find-def : sym list-of-FunDef -> FunDef
(define (find-def fname l)
  (cond
    [(empty? l) (error "no such function")]
    [else (if (symbol=? fname (fundef-fun-name (first l)))
              (first l)
              (find-def fname (rest l)))]))

; É verdade que find-def faz uma busca de complexidade exponencial???

;LR| durante o desenvolvimento é melhor omitir os resultados "good", porque
;LR| quando há muitos testes podemos não ver os "bad" no meio dos "good"
 
(print-only-errors #t)

;LR| O teste abaixo consta do código do Prof. Gubi, mas não funciona porque
;LR| no corpo do lambda o find-def não é invocado (só seria se a função
;LR| definida no lambda fosse invocada), então a exceção esperada não ocorre

;(test/exn (lambda () (find-def 'x empty)) "no such function")

;LR| neste teste a exceção ocorre:
(test/exn (find-def 'x empty) "no such function")


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
    [id (name) (if (symbol=? name sub-id)	; e deste???
                   (num val)				
                   a-wae)]			; a-wae?? isso está correto? 
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

(test (interp (num 5) empty)
      5)
(test (interp (add (num 1) (num 2)) empty)
      3)
(test (interp (sub (num 1) (num 2)) empty)
      -1)
(test (interp (with 'x (add (num 1) (num 17))
                (add (id 'x) (num 12)))
              empty)
      30)

;LR| Aqui o Prof. Gubi colocou a chamada ao iterp dentro de um lambda, e o
;LR| resultado é que este teste não passa porque a exceção não é levantada
;(test/exn (lambda ()
;            (interp (id 'x) empty))
;          "free variable")

;LR| este teste exercita a exceção
(test/exn (interp (id 'x) empty)
          "free variable")

;LR| Novamente a questão do lambda...
;(test/exn (lambda ()
;            (interp (app 'f (num 10)) empty))
;          "no such function")

;LR| este teste exercita a exceção
(test/exn (interp (app 'f (num 10)) empty)
          "no such function")

(test (interp (app 'f (num 10))
              (list (fundef 'f
                            'y
                            (add (id 'y) (num 1)))))
      11)
(test (interp (app 'f (num 10))
              (list
               (fundef 'f
                       'y
                       (with 'y (num 7)
                             (id 'y)))))
      7)
