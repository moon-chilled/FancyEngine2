(defpackage #:vec3
  (:use #:cl)
  (:export :dec :new :vec3))

(in-package :vec3)

(deftype vec3 () '(simple-array single-float (3)))

(defun new (x y z)
  (make-array '(3) :element-type 'single-float :initial-contents (map 'vector (lambda(x)(coerce x'single-float)) (list x y z))))

(defmacro dec-ext (n vecs body)
  (if (cdr vecs)

      `(let ((,(read-from-string (format nil "x~a" n)) (aref ,(car vecs) 0))
             (,(read-from-string (format nil "y~a" n)) (aref ,(car vecs) 1))
             (,(read-from-string (format nil "z~a" n)) (aref ,(car vecs) 2)))
         (dec-ext ,(1+ n) ,(cdr vecs) ,body))

      `(let ((,(read-from-string (format nil "x~a" n)) (aref ,(car vecs) 0))
             (,(read-from-string (format nil "y~a" n)) (aref ,(car vecs) 1))
             (,(read-from-string (format nil "z~a" n)) (aref ,(car vecs) 2)))
             ,@body)))

(defmacro dec (vec &rest body)
  (if (listp vec)

      `(dec-ext ,1 ,vec ,body)

      `(let ((,(read-from-string "x") (aref ,vec 0))
             (,(read-from-string "y") (aref ,vec 1))
             (,(read-from-string "z") (aref ,vec 2)))
         ,@body)))
