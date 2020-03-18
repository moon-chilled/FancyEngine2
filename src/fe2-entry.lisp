(defpackage #:fe2-entry
  (:use #:cl)
  (:export #:main))

(in-package :fe2-entry)

(defparameter *frames* 0)

; maps from [-1, 1] to [0, 1]
(defun normalize (x)
  (/ (+ x 1) 2))

(defun main ()
  (sdl2:with-init (:video :gamecontroller)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 2)
    (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
    (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)

    (sdl2:with-window (win :title "FancyEngine2" :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	(sdl2:with-event-loop (:method :poll)
	  (:idle ()
		 (incf *frames*)
		 (gl:clear-color (normalize (sin (* .017 *frames*))) (normalize (* .017 (cos *frames*))) (normalize (* .5 (+ (sin (* .017 *frames*)) (cos (* .017 *frames*))))) 1)
		 (gl:clear :color-buffer-bit)
		 (gl:flush)
		 (sdl2:gl-swap-window win))

	  (:quit () t))))))
