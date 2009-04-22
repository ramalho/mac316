(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [mul (lhs FAE?) (rhs FAE?)]
  [div (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

; o retorno pode ser um número ou um procedimento
(define (number-or-procedure? v)
  (or (number? v)
      (procedure? v)))

; env tradicional
(define-type Env
  [mtSub]
  [aSub (name symbol?) (value number-or-procedure?) (env Env?)])

;; lookup : symbol Env !number-or-procedure
(define (lookup name env)
  (type-case Env env
             [mtSub () (error ’lookup ”no binding for identifier”)]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-env))]))

;; interpd : FAE Env!number-or-procedure
(define (interpd expr env)
  (type-case FAE expr
             [num (n) n]
             [add (l r) (+ (interpd l env) (interpd r env))]
             [sub (l r) (- (interpd l env) (interpd r env))]
             [mul (l r) (* (interpd l env) (interpd r env))]
             [div (l r) (/ (interpd l env) (interpd r env))]
             [id (v) (lookup v env)]
             [fun (bound-id bound-body)
                  (lambda (arg-val)
                    (interpd bound-body
                             (aSub bound-id arg-val env)))]
             [app (fun-expr arg-expr)
                  (local ([define fun-val (interpd fun-expr env)]
                          [define arg-val (interpd arg-expr env)])
                    (fun-val arg-val))]))

(define (interp expr) (interpd expr (mtSub)))

(load "Parse-FAE.scm")
