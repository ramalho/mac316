;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;LR| Neste arquivo o Prof. Gubi colocou vários comentários e perguntas.
;LR| Os comentários que indicam páginas no livro PLAI e aqueles marcados 
;LR| com ;LR| são meus (Luciano Ramalho)


;LR| Para facilitar a leitura do código, coloquei prefixos nos identificadores
;LR| das variantes dos dois tipos definidos com define-type:
;LR| - tipo FunDef, prefixo $
;LR| - tipo F1WAE, prefixo %

; definição de função
; PLAI section 4.1, p. 28
(define-type FunDef
  [$fundef (fun-name symbol?)
           (arg-name symbol?)
           (body F1WAE?)])	; o que acontece se usarmos WAE aqui???

; expressão aceita pela linguagem
; PLAI section 4.1, p. 28
(define-type F1WAE
  [%num (n number?)]
  [%add (lhs F1WAE?)
        (rhs F1WAE?)]
  [%sub (lhs F1WAE?)
        (rhs F1WAE?)]
  [%with (name symbol?)
         (named-expr F1WAE?)
         (body F1WAE?)]
  [%id (name symbol?)]
  [%app (fun-name symbol?)	; chamada de função
        (arg-expr F1WAE?)])	; melhor F1WAE ou WAE???


; interp : F1WAE list-of-FunDef -> number
; PLAI section 4.1, p. 28-29
(define (interp a-wae defs)
  (type-case F1WAE a-wae
    [%num (n) n]
    [%add (l r) (+ (interp l defs) (interp r defs))]
    [%sub (l r) (- (interp l defs) (interp r defs))]
    [%with (bound-id named-expr body-expr)
      (interp (subst body-expr
                     bound-id
                     ;(%num (interp named-expr defs))) ; LR| errado como no PLAI
                     (interp named-expr defs)) ;LR| corrigido
              defs)]
    [%id (name) (error 'interp "free variable")] ; por que o aviso de erro???
    [%app (fname arg-expr)
          (local [(define f (find-def fname defs))]
            (interp (subst 
                     ($fundef-body f)          ; corpo de f
                     ($fundef-arg-name f)      ; argumento formal de f
                     ;LR| linha abaixo diferente em PLAI
                     (interp arg-expr defs))   ; valor do argumento
                    defs))]
    )
  )


;LR| As linhas marcadas como "diferente em PLAI" estão erradas no livro.
;LR| No primeiro caso, a linha no livro é:
;LR|      (%num (interp named-expr defs)))
;LR| Isso faz com que o interp devolva a representação de número (%num) da 
;LR| nossa linguagem, mas o contrato diz F1WAE list-of-FunDef -> number
;LR| ou seja, o interp tem que devolver números de Scheme. 
;LR| Note que as variantes %add e %sub tratam o resultado da chamada recursiva 
;LR| de interp como número, usando os operadores + e - de Scheme. 
;LR| E a variante %num devolve o valor do campo n de %num, que é um número
;LR| (veja definição de F1WAE).
;LR| A outra linha diferente tem o mesmo erro no livro: constrói um %num.

; find-def : sym list-of-FunDef -> FunDef
(define (find-def fname l)
  (cond
    [(empty? l) (error "no such function")]
    [else (if (symbol=? fname ($fundef-fun-name (first l)))
              (first l)
              (find-def fname (rest l)))]))

; É verdade que find-def faz uma busca de complexidade exponencial???

; subst : F1WAE sym num -> F1WAE
(define (subst a-wae sub-id val)
  (type-case F1WAE a-wae
    [%num (n) a-wae]
    [%add (l r) (%add (subst l sub-id val)
                      (subst r sub-id val))]
    [%sub (l r) (%sub (subst l sub-id val)
                      (subst r sub-id val))]
    [%with (bound-id named-expr body-expr)
      (%with bound-id 
        (subst named-expr sub-id val)
        (if (symbol=? bound-id sub-id)	; qual a razão deste teste???
            body-expr
            (subst body-expr sub-id val)))]
    [%id (name) (if (symbol=? name sub-id)	; e deste???
                    (%num val)				
                    a-wae)]			; a-wae?? isso está correto? 
						; o que tem em a-wae nesse caso???
    [%app (fname arg-expr)
         (%app fname (subst arg-expr sub-id val))])) ; o que faz este subst???


;LR| movi os testes para o arquivo aula3-tests.scm

(include "aula3-tests.scm")