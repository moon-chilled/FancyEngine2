(defpackage #:vec4
  (:use #:cl)
  (:export :dec :new :vec4))

(in-package :vec4)

(deftype vec4 () '(simple-array single-float (4)))

(defun new (x y z w)
  (make-array '(4) :element-type 'single-float :initial-contents (map 'vector (lambda(x)(coerce x'single-float)) (list x y z w))))

(defmacro dec-ext (n vecs body)
  (let ((vec-bind (gensym)))
    `(let ((,vec-bind ,(car vecs)))
       (symbol-macrolet ((,(intern (format nil "X~a" n)) (aref ,vec-bind 0))
                         (,(intern (format nil "Y~a" n)) (aref ,vec-bind 1))
                         (,(intern (format nil "Z~a" n)) (aref ,vec-bind 2))
                         (,(intern (format nil "W~a" n)) (aref ,vec-bind 3))

                         (,(intern (format nil "R~a" n)) (aref ,vec-bind 0))
                         (,(intern (format nil "G~a" n)) (aref ,vec-bind 1))
                         (,(intern (format nil "B~a" n)) (aref ,vec-bind 2))
                         (,(intern (format nil "A~a" n)) (aref ,vec-bind 3)))

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
                             (,(intern "W") (aref ,vec-bind 3))

                             (,(intern "R") (aref ,vec-bind 0))
                             (,(intern "G") (aref ,vec-bind 1))
                             (,(intern "B") (aref ,vec-bind 2))
                             (,(intern "A") (aref ,vec-bind 3)))

              ,@body)))))
