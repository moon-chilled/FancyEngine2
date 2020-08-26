; indirection is needed because 'get-var' and 'set-var' aren't yet defined when
; the prelude is opened
(define vref
  (dilambda
    (lambda (k) (get-var k))
    (lambda (k v) (set-var k v))))

; important for reference:
; (define (my-car x) (car x))
; (set! (setter my-car) (lambda (s v) (log "run with '~a' '~a'" s v) (set! (car s) v)))

(define-macro (log . args)
  `(let* ((msg (format #f ,@args))
          (fn-name (car (*function*)))
          (raw-file-name (cadr (*function*))) ;file name has 'f' appended to it during preprocessing in s7.d:load_getsyms
                                              ;todo: loaded files don't.  This should be done better (maybe check file ext)
          (file-name (substring raw-file-name 0 (1- (length raw-file-name))))
          (line-no (caddr (*function*)))
          )
    (-real-push-log-msg 3
			(string-append (format #f "<lisp>~a:~a (~a): ~a\n" file-name line-no fn-name msg))
			(string-append (format #f "<lisp>~a:~a (~a): ~a\n" file-name line-no fn-name msg)))))

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
