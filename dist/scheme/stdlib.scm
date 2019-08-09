(define (log . args)
  (let ((msg (apply format (cons #f args))))
    (_real_push_log_msg 3
			(string-append "<lisp>" msg)
			(string-append "<lisp>" msg))))
