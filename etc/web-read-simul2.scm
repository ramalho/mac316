;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define-type TipoSessao
  (Sessao (prompt string?)
          (receiver procedure?)))

(define sessoes (make-hash))

#|(define (gen-key hash)
  (cond (hash-ref sessoes (hash-count sessoes) 0)
        (0 hash-count
|#

(define (web-display n)
  (printf "Web output:  ~a ~n" n))

(define (web-read/k p k)
  (begin
    (hash-set! sessoes 42 (Sessao p k)))
    (error 'web-read/k "run (resume 42) to enter number and simulate clicking Submit"))

; The procedure resume uses the values in these boxes to resume the computation:
(define (resume id-sessao)
  (type-case TipoSessao (hash-ref sessoes id-sessao)
             (Sessao (prompt receiver)
                     (begin
                       (display prompt)
                       (receiver (read))))))

(web-read/k "First number: " 
      (lambda (n1)
        (web-read/k "Second number: "
                    (lambda (n2)
                      (web-display
                       (+ n1 n2))))))
