(define (log . args)
  (let ((msg (apply format (cons #f args))))
    (_real_push_log_msg 3
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
