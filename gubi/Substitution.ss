;; Capítulo 3

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [id (name symbol?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
)

;; parse : sexp $\longrightarrow$ WAE
;; to convert s-expressions into WAEs

(define (parse sexp)
  (cond
   [(symbol? sexp) (id sexp)]
   [(number? sexp) (num sexp)]
   [(list? sexp)
    (case (first sexp)
      [(+) (add (parse (second sexp))
                     (parse (third sexp)))]
      [(-) (sub (parse (second sexp))
                     (parse (third sexp)))]
      [(with) (with (first (second sexp))
                         (parse (second (second sexp)))
                         (parse (third sexp)))]
      )
    ]
   )
  )

;; subst : \scheme|WAE| symbol \scheme|WAE| $\rightarrow$ \scheme|WAE|
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case WAE expr
             [num (n) expr]
             [add (l r) (add (subst l sub-id val)
                             (subst r sub-id val))]
             [sub (l r) (sub (subst l sub-id val)
                             (subst r sub-id val))]
             [id (v) (if (symbol=? v sub-id) val expr)]
             [with (bound-id named-expr bound-body)
                   (if (symbol=? bound-id sub-id)
                       (with bound-id
                             (subst named-expr sub-id val)
                             bound-body)
                       (with bound-id
                             (subst named-expr sub-id val)
                             (subst bound-body sub-id val)))]
             )
  )

;; calc : WAE!number
;; evaluates WAE expressions by reducing them to numbers
(define (calc expr)
  (type-case WAE expr
             [num (n) n]
             [add (l r) (+ (calc l) (calc r))]
             [sub (l r) (- (calc l) (calc r))]
             [with (bound-id named-expr bound-body)
                   (calc (subst bound-body
                                bound-id
                                (num (calc named-expr))))]
             [id (v) (error 'calc "free identifier")]
             )
  )