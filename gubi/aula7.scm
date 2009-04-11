;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [mult (lhs RCFAE?) (rhs RCFAE?)]
  [div (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [rec (name symbol?) (body RCFAE?) (expr RCFAE?)]
  [if0 (cond-expr RCFAE?) (yes-expr RCFAE?) (no-expr RCFAE?) ]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)])


; valores de expressões com recursão
(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])

; predicado para saber se temos um RCFAE recursivo
(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))

; novo ambiente, com recursão
(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value RCFAE-Value?)
        (env Env?)]
  [aRecSub (name symbol?)           ; aqui está, a novidade é o boxed value
           (value boxed-RCFAE-Value?)
           (env Env?)])

;; lookup : symbol env! RCFAE-Value
(define (lookup name env)
  (type-case Env env
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-env))]
             [aRecSub (bound-name boxed-bound-value rest-env)
                      (if (symbol=? bound-name name)
                          (unbox boxed-bound-value)
                          (lookup name rest-env))]))

;; cyclically-bind-and-interp : symbol RCFAE env! env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]  ; o (numV 1729) é qualquer coisa
          [define new-env (aRecSub bound-id value-holder env)] ; cria o env com a associação falsa
          [define named-expr-val (interpd named-expr new-env)]) ; cria o valor correto, com  a closureV
    (begin
      (set-box! value-holder named-expr-val)  ; corrige o ambiente (new-env)
      new-env)))                              ; retorna o ambiente corrigido


(include "Parse-RFCAE.scm")

(define (num-op op l r) (numV (op (numV-n l) (numV-n r))))

(define (num+ l r)  (num-op + l r))
(define (num- l r)  (num-op - l r))
(define (num* l r)  (num-op * l r))
(define (num/ l r)  (num-op / l r))


(define (num-zero? n) (zero? (numV-n n)))


;; interpd : RCFAE env! RCFAE-Value
(define (interpd expr env)
  (type-case RCFAE expr
             [num (n) (numV n)]
             [add (l r) (num+ (interpd l env) (interpd r env))]
             [sub (l r) (num- (interpd l env) (interpd r env))]
             [div (l r) (num/ (interpd l env) (interpd r env))]             
             [mult (l r) (num* (interpd l env) (interpd r env))]
             [if0 (test truth falsity)
                  (if (num-zero? (interpd test env))
                      (interpd truth env)
                      (interpd falsity env))]
             [id (v) (lookup v env)]
             [fun (bound-id bound-body)
                  (closureV bound-id bound-body env)]
             [app (fun-expr arg-expr)
                  (local ([define fun-val (interpd fun-expr env)])
                    (interpd (closureV-body fun-val)
                            (aSub (closureV-param fun-val)
                                  (interpd arg-expr env)
                                  (closureV-env fun-val))))]
             [rec (bound-id named-expr bound-body)
               (interpd bound-body
                       (cyclically-bind-and-interp bound-id
                                                   named-expr
                                                   env))]))

(define (interp expr) (interpd expr (mtSub)))
