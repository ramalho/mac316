;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
(define the-receiver (box 'dummy-value))
(define receiver-prompt (box 'dummy-value))

(define (web-display n)
  (printf "Web output:  ~a ~n" n))

(define (web-read/k p k)
  (begin
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)))
    ; (error 'web-read/k "run (resume) to enter number and simulate clicking Submit")))

; The procedure resume uses the values in these boxes to resume the computation:
(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

(web-read/k "First number: " 
      (lambda (n1)
        (web-read/k "Second number: "
                    (lambda (n2)
                      (web-display
                       (+ n1 n2))))))
