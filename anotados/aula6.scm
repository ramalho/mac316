;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; expressão aceita pela linguagem
(define-type CFAE/L
  [%num  (n number?)]
  [%add  (lhs CFAE/L?) (rhs CFAE/L?)]
  [%sub  (lhs CFAE/L?) (rhs CFAE/L?)]
  [%mul  (lhs CFAE/L?) (rhs CFAE/L?)]
  [%div  (lhs CFAE/L?) (rhs CFAE/L?)]
  [%fun  (param symbol?)  (body CFAE/L?)]
  [%id   (name symbol?)]
  [%app  (fun-expr CFAE/L?) (arg-expr CFAE/L?)]
  [%if0  (cond CFAE/L?) (e-yes CFAE/L?) (e-no CFAE/L?)] ;condicional simples 
  )

;; Tabela de simbolos
(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Env?)]
  )

; busca linear
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier ~s" name)]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value		; valor associado
              (lookup name rest-env) ; procura no resto
              )
          ]
    )
  )


;; Valor de retorno
(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)]
  )

; interp : CFAE/L -> CFAE/L
(define (interp expr) (interpd expr [mtSub]))

; interpd : CFAE/L env -> CFAE/L
(define (interpd expr env)
  (type-case CFAE/L expr
    [%num (n) (numV n)]
    [%add (l r) (num+ (interpd l env) (interpd r env))]
    [%sub (l r) (num- (interpd l env) (interpd r env))]
    [%mul (l r) (num* (interpd l env) (interpd r env))]
    [%div (l r) (num/ (interpd l env) (interpd r env))]
    [%id (name) (lookup name env)]
    [%fun (arg body) (closureV arg body env)]
    [%app (fun-expr arg-expr)
         (local ([define f (strict (interpd fun-expr env))]
                 [define arg (exprV arg-expr env)]) ; troca pela expressão!
           (interpd (closureV-body f) 
                    (aSub (closureV-param f) 
                          arg                   ;lazy!!!!!!
                          (closureV-env f)))
           )
         ]
    [%if0 (e1 ey en) 
         (if (num-zero? (interpd e1 env)) 
             (interpd ey env) ; sim
             (interpd en env) ; não
             )
         ] 
    ))

(define (tonum nn) (numV-n nn))


(define (num-op op l r) (numV (op (numV-n (strict l)) (numV-n (strict r)))))

(define (num+ l r)  (num-op + l r))
(define (num- l r)  (num-op - l r))
(define (num* l r)  (num-op * l r))
(define (num/ l r)  (num-op / l r))


(define (num-zero? n) (zero? (numV-n (strict n))))

(define VERBOSE #f)

;; strict CFAE/L-Value->CFAE/L-Value [sem exprV]
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (local ([define val (strict (interpd expr env))])
             (begin
               (cond (VERBOSE (printf "Forçando exprV com ~a~n" val)))
               val))
           ]
    [else e]
    ))


; Rotina auxiliar

(include "Parse-FAE.scm")

;; Testes movidos para aula6-tests.scm
