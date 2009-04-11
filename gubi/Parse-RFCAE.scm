; Rotina auxiliar
(define (read-prog arq) (call-with-input-file arq (lambda (a) (read a))))

; Parser
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
	   [(*) (mult (parse (second sexp))
				 (parse (third sexp)))]
	   [(/) (div (parse (second sexp))
				 (parse (third sexp)))]
	   [(with) (app (fun (first (second sexp)) ; param
						 (parse (third sexp))) ;body
					(parse (second (second sexp))) ;valor
					)
		]
	   [(if0) (if0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))]
	   [(fun) (fun (first (second sexp)) (parse (third sexp)))]
	   [(rec) (rec (first (second sexp)) 
                   (parse (second (second sexp))) 
                   (parse (third sexp)))]
	   [(call) (app (parse (second sexp)) (parse (third sexp)))]
	   [else (app (parse (first sexp)) (parse (second sexp)))]
	   )
	 ]
))