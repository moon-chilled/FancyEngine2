(defpackage #:vec4
  (:use #:cl)
  (:export :dec :new :vec4))

(in-package :vec4)

(deftype vec4 () '(simple-array single-float (4)))

(defun new (x y z w)
  (make-array '(4) :element-type 'single-float :initial-contents (map 'vector (lambda(x)(coerce x'single-float)) (list x y z w))))

(defmacro dec-ext (n vecs body)
  (if (cdr vecs)

      `(let ((,(read-from-string (format nil "x~a" n)) (aref ,(car vecs) 0))
             (,(read-from-string (format nil "y~a" n)) (aref ,(car vecs) 1))
             (,(read-from-string (format nil "z~a" n)) (aref ,(car vecs) 2))
             (,(read-from-string (format nil "w~a" n)) (aref ,(car vecs) 3)))
         (dec-ext ,(1+ n) ,(cdr vecs) ,body))

      `(let ((,(read-from-string (format nil "x~a" n)) (aref ,(car vecs) 0))
             (,(read-from-string (format nil "y~a" n)) (aref ,(car vecs) 1))
             (,(read-from-string (format nil "z~a" n)) (aref ,(car vecs) 2))
             (,(read-from-string (format nil "w~a" n)) (aref ,(car vecs) 3)))
             ,@body)))

(defmacro dec (vec &rest body)
  (if (listp vec)

      `(dec-ext ,1 ,vec ,body)

      `(let ((,(read-from-string "x") (aref ,vec 0))
             (,(read-from-string "y") (aref ,vec 1))
             (,(read-from-string "z") (aref ,vec 2))
             (,(read-from-string "w") (aref ,vec 3)))
         ,@body)))
