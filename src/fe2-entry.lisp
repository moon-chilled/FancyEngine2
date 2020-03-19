(defpackage #:fe2-entry
  (:use #:cl)
  (:export #:main))

(in-package :fe2-entry)

(defparameter *frames* 0)

; maps from [-1, 1] to [0, 1]
(defun normalize (x)
  (/ (+ x 1) 2))

(defun main ()
  (win:init)

  (vec2:dec ((vec2:new 5 6) (vec2:new 8 9))
	    (format t "I have (~a,~a) and (~a,~a)" x1 y1 x2 y2))

  (loop :for i :from 1 :to 30 :do
       (incf *frames*)
       (gfx:clear (normalize (sin (* .017 *frames*))) (normalize (* 0.017 (cos *frames*))) (normalize (* .5 (+ (sin (* .017 *frames*)) (cos (* .017 *frames*))))) 1)
       (gfx:blit)
       (win:blit))

  (win:quit))