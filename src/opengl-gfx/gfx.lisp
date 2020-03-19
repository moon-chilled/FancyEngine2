(defpackage #:gfx
  (:use #:cl)
  (:export :clear :blit))

(in-package :gfx)

(defun clear (r g b a)
  (gl:clear-color r g b a)
  (gl:clear :color-buffer-bit))

(defun blit () (gl:flush))
