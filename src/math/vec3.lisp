(defpackage #:vec3
  (:use #:cl)
  (:export :dec :new :vec3))

(in-package :vec3)

(deftype vec3 () '(simple-array single-float (3)))

(defun new (x y z)
  (make-array '(3) :element-type 'single-float :initial-contents (map 'vector (lambda(x)(coerce x'single-float)) (list x y z))))

(defmacro dec-ext (n vecs body)
  (let ((vec-bind (gensym)))
    `(let ((,vec-bind ,(car vecs)))
       (symbol-macrolet ((,(intern (format nil "X~a" n)) (aref ,vec-bind 0))
                         (,(intern (format nil "Y~a" n)) (aref ,vec-bind 1))
                         (,(intern (format nil "Z~a" n)) (aref ,vec-bind 2))

                         (,(intern (format nil "R~a" n)) (aref ,vec-bind 0))
                         (,(intern (format nil "G~a" n)) (aref ,vec-bind 1))
                         (,(intern (format nil "B~a" n)) (aref ,vec-bind 2)))

         ,(if (cdr vecs)
              `(dec-ext ,(1+ n) ,(cdr vecs) ,body)
              `(progn ,@body))))))

(defmacro dec (vec &rest body)
  (if (listp vec)

      `(dec-ext ,1 ,vec ,body)

      (let ((vec-bind (gensym)))
        `(let ((,vec-bind ,vec))
           (symbol-macrolet ((,(intern "X") (aref ,vec-bind 0))
                             (,(intern "Y") (aref ,vec-bind 1))
                             (,(intern "Z") (aref ,vec-bind 2))

                             (,(intern "R") (aref ,vec-bind 0))
                             (,(intern "G") (aref ,vec-bind 1))
                             (,(intern "B") (aref ,vec-bind 2)))

              ,@body)))))
