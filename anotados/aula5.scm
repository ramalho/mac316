
; expressão aceita pela linguagem
(define-type FAE
  [%num  (n number?)]
  [%add  (lhs FAE?) (rhs FAE?)]
  [%sub  (lhs FAE?) (rhs FAE?)]
  [%mul  (lhs FAE?) (rhs FAE?)]
  [%div  (lhs FAE?) (rhs FAE?)]
  [%fun  (param symbol?)  (body FAE?)]
  [%id   (name symbol?)]
  [%app  (fun-expr FAE?) (arg-expr FAE?)]
  [%if0  (cond FAE?) (e-yes FAE?) (e-no FAE?)] ;condicional simples 
  )

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
            (ds DefrdSub?)] ; inclui a tabela capturada
)


; interp : FAE -> FAE
(define (interp expr) (interpd expr [mtSub]))

; interpd : FAE ds -> FAE
(define (interpd expr ds )
  (type-case FAE expr
             [%num (n) (numV n)]
             [%add (l r) (num+ (interpd l ds) (interpd r ds))]
             [%sub (l r) (num- (interpd l ds) (interpd r ds))]
             [%mul (l r) (num* (interpd l ds) (interpd r ds))]
             [%div (l r) (num/ (interpd l ds) (interpd r ds))]
             [%id (name) (lookup name ds)]
             [%fun (arg body) (closureV arg body ds)]
             [%app (fun-expr arg-expr)
                  (local [(define f (interpd fun-expr ds))]
                    (interpd (closureV-body f) 
                             (aSub (closureV-param f) 
                                   (interpd arg-expr ds) 
                                   (closureV-ds f)))
                    )
                  ]
             [%if0 (e1 ey en) (if (num-zero? (interpd e1 ds)) (interpd ey ds) (interpd en ds))]
             )
  )

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
(define (num-zero? n) (= 0 (numV-n n)))
; Rotina auxiliar
(include "Parse-FAE.scm")

;;; Testes em aula5-tests.scm
