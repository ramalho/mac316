; definição de função
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body FWAE?)])

; expressão aceita pela linguagem 
(define-type FWAE
  [num  (n number?)]
  [add  (lhs FWAE?) (rhs FWAE?)]
  [sub  (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?)   (named-expr FWAE?) (body FWAE?)]
  [fun  (param symbol?)  (body FWAE?)]
  [id   (name symbol?)]
  [app  (fun-expr FWAE?) (arg-expr FWAE?)])


; interp : FWAE  -> FWAE
(define (interp a-fwae )
  (type-case FWAE a-fwae
    [num (n) a-fwae]  ; agora pode retornar as duas coisas, é bom deixar o "tipo"
    [add (l r) (add-numbers (interp l) (interp r))]
    [sub (l r) (sub-numbers (interp l) (interp r))]
    [with (bound-id named-expr body-expr)
      (interp (subst body-expr
                     bound-id
                     (interp named-expr))
              )]
    [id (name) (error 'interp "free variable")]
    [fun (arg body) a-fwae]
    [app (fun-expr arg-expr)
         (local [(define f (interp fun-expr))]
           (interp (subst (fun-body f)
                          (fun-param f)
                          (interp arg-expr))
                   ))]))

; subst : FWAE sym num -> FWAE
(define (subst a-wae sub-id val)
  (type-case FWAE a-wae
    [num (n) a-wae]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr body-expr)
      (with bound-id 
        (subst named-expr sub-id val)
        (if (symbol=? bound-id sub-id)
            body-expr
            (subst body-expr sub-id val)))]
    [fun (name body) 
         (if (symbol=? name sub-id)
             body
             (subst body sub-id val))]
    [id (name) (if (symbol=? name sub-id)
                   val			
                   (interp a-wae))]
    [app (fexpr arg-expr)
         (app (subst fexpr sub-id val) (subst arg-expr sub-id val))]))

(define (tonum nn) 
  (type-case FWAE nn
             [num (n) n]
             [else (error "Bad num")]
             ))
                    
(define (add-numbers l r) 
  (num (+ (tonum l) (tonum r))))

(define (sub-numbers l r) 
  (num (- (num-n l) (num-n r))))
