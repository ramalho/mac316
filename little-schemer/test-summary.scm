; exibir contagem de falhas, exceções e testes
(define (contar-testes simbolo) 
  (length (filter (lambda (teste) (eq? simbolo (car teste))) 
                  plai-all-test-results)))

(display (list (contar-testes 'bad) "falhas," 
               (contar-testes 'exception) "excecoes em"
               (length plai-all-test-results) "testes"))
