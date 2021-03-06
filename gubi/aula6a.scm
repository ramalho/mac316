;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; expressão aceita pela linguagem
(define-type CFAE/L
  [num  (n number?)]
  [add  (lhs CFAE/L?) (rhs CFAE/L?)]
  [sub  (lhs CFAE/L?) (rhs CFAE/L?)]
  [mul  (lhs CFAE/L?) (rhs CFAE/L?)]
  [div  (lhs CFAE/L?) (rhs CFAE/L?)]
  [fun  (param symbol?)  (body CFAE/L?)]
  [id   (name symbol?)]
  [app  (fun-expr CFAE/L?) (arg-expr CFAE/L?)]
  [if0  (cond CFAE/L?) (e-yes CFAE/L?) (e-no CFAE/L?)] ;condicional simples 
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
         (env Env?)
         (cache boxed-boolean/CFAE/L-Value?)]
)

(define (boxed-boolean/CFAE/L-Value? v)
  (and (box? v)
       (or (boolean? (unbox v))
           (numV? (unbox v))
           (closureV? (unbox v)))))

; interp : CFAE/L -> CFAE/L
(define (interp expr) (interpd expr [mtSub]))

; interpd : CFAE/L env -> CFAE/L
(define (interpd expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interpd l env) (interpd r env))]
    [sub (l r) (num- (interpd l env) (interpd r env))]
    [mul (l r) (num* (interpd l env) (interpd r env))]
    [div (l r) (num/ (interpd l env) (interpd r env))]
    [id (name) (lookup name env)]
    [fun (arg body) (closureV arg body env)]
    [app (fun-expr arg-expr)
         (local ([define f (strict (interpd fun-expr env))]
                 [define arg (exprV arg-expr env (box false))]) ; troca pela expressão!
           (interpd (closureV-body f) 
                    (aSub (closureV-param f) 
                          arg                   ;lazy!!!!!!
                          (closureV-env f)))
           )
         ]
    [if0 (e1 ey en) (if (num-zero? (interpd e1 env)) (interpd ey env) (interpd en env))]
    ))

(define (tonum nn) (numV-n nn))


(define (num-op op l r) (numV (op (numV-n (strict l)) (numV-n (strict r)))))

(define (num+ l r)  (num-op + l r))
(define (num- l r)  (num-op - l r))
(define (num* l r)  (num-op * l r))
(define (num/ l r)  (num-op / l r))


(define (num-zero? n) (zero? (numV-n (strict n))))

;; strict CFAE/L-Value->CFAE/L-Value [sem exprV]
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env cache)
           (if (boolean? (unbox cache))
               (local ([define val (strict (interpd expr env))])
                 (begin
                   (printf "Forçando exprV ~a com ~a~n" expr val)
                   (set-box! cache val)
                   val))
               (begin
                 (printf "Usando o valor guardado~n" )
                 (unbox cache)))
           ]
    [else e]
    ))


; Rotina auxiliar

(include "Parse-FAE.scm")


(test (tonum 
       (interpd (parse 
                 '(with (k (fun (x) (+ 3 x))) (+ 4 (call k 2))))  
                (mtSub)))
      9)

(test (parse 
       '{with {x 3} {fun (y) {+ x y}}}) 
      (app (fun 'x (fun 'y (add (id 'x) (id 'y)))) (num 3)))


(test (tonum (interp (parse '{with {f {fun (x) (+ x 1)}} {f 2}}))) 3)

(test (parse '{with {x 3}
                    {fun (y) {+ x y}}}) 
      (app (fun 'x (fun 'y (add (id 'x) (id 'y)))) (num 3)))

(interp 
 (parse 
  '(with 
    (dq (with (d/dx (fun (f) 
                         (fun (x) 
                              (/(-(f (+ x 0.00001)) (f x)) 0.00001))))
              (d/dx (fun (x) (* x x))))) 
    (dq 9))))