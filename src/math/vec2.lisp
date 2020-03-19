(defpackage #:vec2
  (:use #:cl)
  (:export :dec :new :vec2))

(in-package :vec2)

(deftype vec2 () '(simple-array single-float (2)))

(defun new (x y)
  (make-array '(2) :element-type 'single-float :initial-contents (map 'vector (lambda(x)(coerce x'single-float)) (list x y))))

(defmacro dec-ext (n vecs body)
  (if (cdr vecs)

      `(let ((,(read-from-string (format nil "x~a" n)) (aref ,(car vecs) 0))
             (,(read-from-string (format nil "y~a" n)) (aref ,(car vecs) 1)))
         (dec-ext ,(1+ n) ,(cdr vecs) ,body))

      `(let ((,(read-from-string (format nil "x~a" n)) (aref ,(car vecs) 0))
             (,(read-from-string (format nil "y~a" n)) (aref ,(car vecs) 1)))
             ,@body)))

(defmacro dec (vec &rest body)
  (if (listp vec)

      `(dec-ext ,1 ,vec ,body)

      `(let ((,(read-from-string "x") (aref ,vec 0))
             (,(read-from-string "y") (aref ,vec 1)))
         ,@body)))
