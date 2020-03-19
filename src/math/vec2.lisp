(defpackage #:vec2
  (:use #:cl)
  (:export :dec :new :vec2))

(in-package :vec2)

(deftype vec2 () '(simple-array single-float (2)))

(defun new (x y)
  (make-array '(2) :element-type 'single-float :initial-contents (map 'vector (lambda(x)(coerce x'single-float)) (list x y))))

(defmacro dec-ext (n vecs body)
  (let ((vec-bind (gensym)))
    `(let ((,vec-bind ,(car vecs)))
       (symbol-macrolet ((,(intern (format nil "X~a" n)) (aref ,vec-bind 0))
                         (,(intern (format nil "Y~a" n)) (aref ,vec-bind 1)))

         ,(if (cdr vecs)
              `(dec-ext ,(1+ n) ,(cdr vecs) ,body)
              `(progn ,@body))))))

(defmacro dec (vec &rest body)
  (if (listp vec)

      `(dec-ext ,1 ,vec ,body)

      (let ((vec-bind (gensym)))
        `(let ((,vec-bind ,vec))
           (symbol-macrolet ((,(intern "X") (aref ,vec-bind 0))
                              (,(intern "Y") (aref ,vec-bind 1)))
              ,@body)))))
