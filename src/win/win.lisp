(defpackage #:win
  (:use #:cl)
  (:export :init :blit :quit))

(in-package :win)

(defparameter *window* nil)

(defun init ()
  (sdl2:init :video :gamecontroller)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)                                                     
  (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
  (setf *window* (sdl2:create-window :title "FancyEngine2" :flags '(:shown :opengl)))
  (sdl2:gl-create-context *window*))

(defun blit ()
  (sdl2:gl-swap-window *window*))

(defun quit ()
  (sdl2:destroy-window *window*)
  (sdl2:quit))
