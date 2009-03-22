; expressão aceita pela linguagem
(define-type FAE
  [num  (n number?)]
  [add  (lhs FAE?) (rhs FAE?)]
  [sub  (lhs FAE?) (rhs FAE?)]
  [mul  (lhs FAE?) (rhs FAE?)]
  [div  (lhs FAE?) (rhs FAE?)]
  [fun  (param symbol?)  (body FAE?)]
  [id   (name symbol?)]
  [app  (fun-expr FAE?) (arg-expr FAE?)])

;; Tabela de simbolos
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)]
)

; busca linear
(define (lookup name ds)
  (type-case DefrdSub ds
             [mtSub () (error 'lookup "no binding for identifier ~s" name)]
             [aSub (bound-name bound-value rest-ds)
                   (if (symbol=? bound-name name)
                       bound-value		; valor associado
                       (lookup name rest-ds) ; procura no resto
                       )
                   ]
             )
  )


;; Valor de retorno
(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (ds DefrdSub?)]
)


; interp : FAE -> FAE
(define (interp expr) (interpd expr [mtSub]))

; interpd : FAE ds -> FAE
(define (interpd expr ds )
  (type-case FAE expr
             [num (n) (numV n)]
             [add (l r) (num+ (interpd l ds) (interpd r ds))]
             [sub (l r) (num- (interpd l ds) (interpd r ds))]
             [mul (l r) (num* (interpd l ds) (interpd r ds))]
             [div (l r) (num/ (interpd l ds) (interpd r ds))]
             [id (name) (lookup name ds)]
             [fun (arg body) (closureV arg body ds)]
             [app (fun-expr arg-expr)
                  (local [(define f (interpd fun-expr ds))]
                    (interpd (closureV-body f) 
                             (aSub (closureV-param f) 
                                   (interpd arg-expr ds) 
                                   (closureV-ds f)))
                    )
                  ]
             ))

(define (tonum nn) (numV-n nn))


; (define (num+ l r)
;    (numV (+ (tonum l) (tonum r))))

;(define (num- l r) 
;    (numV (- (tonum l) (tonum r))))

(define (num-op op l r) (numV (op (numV-n l) (numV-n r))))

(define (num+ l r)  (num-op + l r))
(define (num- l r)  (num-op - l r))
(define (num* l r)  (num-op * l r))
(define (num/ l r)  (num-op / l r))

; Rotina auxiliar

(load "Parse-FAE.scm")


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