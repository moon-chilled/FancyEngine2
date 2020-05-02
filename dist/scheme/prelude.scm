; indirection is needed because 'get-var' and 'set-var' aren't yet defined when
; the prelude is opened
(define vref
  (dilambda
    (lambda (k) (get-var k))
    (lambda (k v) (set-var k v))))

; important for reference:
; (define (my-car x) (car x))
; (set! (setter my-car) (lambda (s v) (log "run with '~a' '~a'" s v) (set! (car s) v)))

(define (log . args)
  (let ((msg (apply format (cons #f args))))
    (-real-push-log-msg 3
			(string-append "<lisp>" msg "\n")
			(string-append "<lisp>" msg "\n"))))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define-macro (incf x . c)
	      `(begin
		 (set! ,x (+ ,x
			     ,(if (null? c)
				  1
				  (car c))))
		 ,x))

(define-macro (decf x . c)
	      `(begin
		 (set! ,x (- ,x
			     ,(if (null? c)
				  1
				  (car c))))
		 ,x))
