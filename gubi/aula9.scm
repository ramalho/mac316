;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
;; tipo da expressao
(define-type RVCFAE
  [num   (n number?)]
  [add   (lhs RVCFAE?) (rhs RVCFAE?)]
  [sub   (lhs RVCFAE?) (rhs RVCFAE?)]
  [mult  (lhs RVCFAE?) (rhs RVCFAE?)]
  [div   (lhs RVCFAE?) (rhs RVCFAE?)]
  [id    (name symbol?)]
  [fun   (param symbol?) (body RVCFAE?)]
  [refun (param symbol?) (body RVCFAE?)]
  [if0   (cond-expr RVCFAE?) (yes-expr RVCFAE?) (no-expr RVCFAE?) ]
  [app   (fun-expr RVCFAE?) (arg-expr RVCFAE?)]
  [set   (var symbol?) (value RVCFAE?)]
  [seqn  (e1 RVCFAE?) (e2 RVCFAE?)]
  )


;; Environment
(define-type Env
  [mtSub]
  [aSub (name symbol?) (location number?) (env Env?)]
)

;; Store
(define-type Store
  [mtSto]
  [aSto (location number?) (value RVCFAE-Value?) (sto Store?)]
)

; busca linear no env
(define (env-lookup name env)
  (type-case Env env
             [mtSub () (error 'env-lookup "no binding for identifier ~s" name)]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value		; valor associado
                       (env-lookup name rest-env) ; procura no resto
                       )
                   ]
             )
  )
; valores de expressões
(define-type RVCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RVCFAE?) (env Env?)]
  [refclosV (param symbol?) (body RVCFAE?) (env Env?)]
  )

; busca linear no store
(define (store-lookup loc-index sto)
  (type-case Store sto
             [mtSto () (error 'store-lookup "no value at location ~s" loc-index)]
             [aSto (location value rest-sto)
                   (if (= location loc-index)
                       value		; valor associado
                       (store-lookup loc-index rest-sto) ; procura no resto
                       )
                   ]
             )
  )

;; tipo simples que contém um valor e um "store"
(define-type Value*Store
  [v*s (value RVCFAE-Value?) (store Store?)])


;; retorna a próxima localização disponível
(define next-location
  (local ([define last-loc (box -1)])
    (lambda (store) 
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

;; interpd : RVCFAE Env Store ->Value*Store
(define (interpd expr env store)
  (type-case RVCFAE expr         
    [num (n) (v*s (numV n) store)]
    [add (l r)
         (type-case Value*Store (interpd l env store)
               [v*s (l-value l-store)
                    (type-case Value*Store (interpd r env l-store)
                          [v*s (r-value r-store)
                               (v*s (num+ l-value r-value)
                                                  r-store)])])] ;r-store, claro
    [sub (l r)
         (type-case Value*Store (interpd l env store)
               [v*s (l-value l-store)
                    (type-case Value*Store (interpd r env l-store)
                          [v*s (r-value r-store)
                               (v*s (num- l-value r-value)
                                                  r-store)])])]
    [mult (l r)
          (type-case Value*Store (interpd l env store)
                [v*s (l-value l-store)
                     (type-case Value*Store (interpd r env l-store)
                           [v*s (r-value r-store)
                                (v*s (num* l-value r-value)
                                                   r-store)])])]
     [div (l r)
          (type-case Value*Store (interpd l env store)
                [v*s (l-value l-store)
                     (type-case Value*Store (interpd r env l-store)
                           [v*s (r-value r-store)
                                (v*s (num/ l-value r-value)
                                                   r-store)])])]
      [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
      [fun (bound-id bound-body)
           (v*s (closureV bound-id bound-body env) store)]
      [refun (bound-id bound-body) ;quase igual a fun
             (v*s (refclosV bound-id bound-body env) store)]
             ;; Aqui estão as grandes modificações
      [app (fun-expr arg-expr) 	; função e argumento
           (type-case Value*Store (interpd fun-expr env store) ; árvore da função
                 [v*s (fun-value fun-store) ; função e store
                      (type-case RVCFAE-Value fun-value
                            [closureV (cl-param cl-body cl-env)
                                ; primeiro o argumento passado
                               (type-case Value*Store (interpd arg-expr env fun-store)
                                     [v*s (arg-value arg-store)
                                     ;; nova localização (passagem por valor)
                                        (local ([define new-loc 
                                                        (next-location arg-store)])
                                                (interpd cl-body
                                                        (aSub cl-param
                                                              new-loc
                                                              cl-env)
                                                        (aSto new-loc
                                                              arg-value
                                                              arg-store)))]
                                                                  )]
                             [refclosV (cl-param cl-body cl-env)
                                      ; posição do argumento arg-expr 
                                      ;(que ? "(id '...)")
                                      (local ([define arg-loc 
                                                      (env-lookup (id-name arg-expr)
                                                                   env)])
                                             (interpd cl-body
                                                  (aSub cl-param
                                                        arg-loc
                                                        cl-env)
                                                     fun-store)
                                                         )]
                            [numV (n) (error 'interpd "Tentando chamar um número!")]
                                             )])]
             
      [if0 (test truth falsity)
           (type-case Value*Store (interpd test env store)
                 [v*s (test-value test-store)
                      (if (num-zero? test-value)
                          (interpd truth env test-store)
                          (interpd falsity env test-store))])]
      [set (var value)
           (type-case Value*Store (interpd value env store) ; valor!!
                 [v*s (value-value value-store)
                      (local ([define pos (env-lookup var env)])
                            (v*s value-value
                                 (aSto pos
                                       value-value
                                       value-store)))])]
       [seqn (e1 e2)
             (type-case Value*Store (interpd e1 env store)
                   [v*s (e1-value e1-store)
                        (interpd e2 env e1-store)])]))

(define (num-op op l r) (numV (op (numV-n l) (numV-n r))))

(define (num+ l r)  (num-op + l r))
(define (num- l r)  (num-op - l r))
(define (num* l r)  (num-op * l r))
(define (num/ l r)  (num-op / l r))


(define (num-zero? n) (zero? (numV-n n)))

(define (interp expr) 
  (type-case Value*Store (interpd expr (mtSub)  (mtSto))
             [v*s (value store) value]))

(include "Parse-RVCFAE.scm")

(test 
     (interp (parse '{with {v 0} {with {f {fun {y} {set y 5}}} {seqn {f v} v}}}))
     (numV 0))
(test
     (interp (parse '{with {v 0} {with {f {refun {y} {set y 5}}} {seqn {f v} v}}}))
     (numV 5))