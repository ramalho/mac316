(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [mul (lhs FAE?) (rhs FAE?)]
  [div (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [if0 (cond-expr FAE?) (yes-expr FAE?) (no-expr FAE?) ]
  [app (fun-expr FAE?) (arg-expr FAE?)])

; valores de expressões
(define-type FAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]			; fun é lambda!
)

; Env é agora uma função
(define (Env?  x) (procedure? x))

(define (mtSub) (lambda (name)  (error ’lookup ”no binding for identifier”)))

;; aSub: symbol FAE-Value Env!Env
(define (aSub bound-name bound-value env)
  (lambda (want-name)
	(cond
	 [(symbol=? want-name bound-name) bound-value]
	 [else (lookup want-name env)])))

;; lookup : symbol Env! FAE-Value
(define (lookup name env) (env name))


(load "Parse-FAE.scm")

(define (num-op op l r) (numV (op (numV-n l) (numV-n r))))

(define (num+ l r)  (num-op + l r))
(define (num- l r)  (num-op - l r))
(define (num* l r)  (num-op * l r))
(define (num/ l r)  (num-op / l r))


(define (num-zero? n) (zero? (numV-n n)))


;; interpd : FAE env! FAE-Value
(define (interpd expr env)
  (type-case FAE expr
             [num (n) (numV n)]
             [add (l r) (num+ (interpd l env) (interpd r env))]
             [sub (l r) (num- (interpd l env) (interpd r env))]
             [div (l r) (num/ (interpd l env) (interpd r env))]             
             [mul (l r) (num* (interpd l env) (interpd r env))]
             [if0 (test truth falsity)
                  (if (num-zero? (interpd test env))
                      (interpd truth env)
                      (interpd falsity env))]
             [id (v) (lookup v env)]
             [fun (bound-id bound-body)
                  (closureV (lambda (arg-val)
			  (interpd bound-body (aSub bound-id arg-val env))))]
             [app (fun-expr arg-expr)
                  (local ([define fun-val (interpd fun-expr env)]
                          [define arg-val (interpd arg-expr env)])
                    ((closureV-p fun-val) arg-val))]))

(define (interp expr) (interpd expr (mtSub)))

(define (interpv expr) 
    (local ([define res (interp expr)])
      (cond
        [(numV? res) (numV-n res)]
        [(closureV? res) (closureV-p res)]
        [else (printf "Isso não deveria ter acontecido~n")])))

                         
                         
                         
                         
                         
                         
                         
                         
                         