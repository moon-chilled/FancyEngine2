(define (vec2 x y)
  (float-vector x y))

(define (vec3 x y z)
  (float-vector x y z))

; Generate and (n)ormalize a vector
(define (vec3n x y z)
  (vec3-normalize (float-vector x y z)))
(define (vec2n x y z)
  (vec2-normalize (float-vector x y)))

; todo: dilambda/setter
(define (.x vec) (float-vector-ref vec 0))
(define (.r vec) (float-vector-ref vec 0))
(define (.y vec) (float-vector-ref vec 1))
(define (.g vec) (float-vector-ref vec 1))
(define (.z vec) (float-vector-ref vec 2))
(define (.b vec) (float-vector-ref vec 2))

(define-macro (vec3-dec-ext n vecs body)
              (let ((vec-bind (gensym)))
                `(let ((,vec-bind ,(car vecs)))
                   (let ((,(string->symbol (format #f "x~a" n)) (float-vector-ref ,vec-bind 0))
                         (,(string->symbol (format #f "y~a" n)) (float-vector-ref ,vec-bind 1))
                         (,(string->symbol (format #f "z~a" n)) (float-vector-ref ,vec-bind 2)))
                     ,(if (null? (cdr vecs))
                        `(begin ,@body)
                        `(vec3-dec-ext ,(1+ n) ,(cdr vecs) ,body))))))

(define-macro (vec3-dec vec . body)
              ; (vec3-dec $x), which is sugar for (vref "x") shouldn't go to ext
              (if (and (list? vec) (not (eq? (car vec) 'vref)))
                `(vec3-dec-ext 0 ,vec ,body)
                (let ((vec-bind (gensym)))
                  `(let ((,vec-bind ,vec))
                     (let ((x (float-vector-ref ,vec-bind 0))
                           (y (float-vector-ref ,vec-bind 1))
                           (z (float-vector-ref ,vec-bind 2)))
                       ,@body)))))

(define-macro (vec2-dec-ext n vecs body)
              (let ((vec-bind (gensym)))
                `(let ((,vec-bind ,(car vecs)))
                   (let ((,(string->symbol (format #f "x~a" n)) (float-vector-ref ,vec-bind 0))
                         (,(string->symbol (format #f "y~a" n)) (float-vector-ref ,vec-bind 1)))
                     ,(if (null? (cdr vecs))
                        `(begin ,@body)
                        `(vec2-dec-ext ,(1+ n) ,(cdr vecs) ,body))))))

(define-macro (vec2-dec vec . body)
              (if (and (list? vec) (not (eq? (car vec) 'vref)))
                `(vec2-dec-ext 0 ,vec ,body)
                (let ((vec-bind (gensym)))
                  `(let ((,vec-bind ,vec))
                     (let ((x (float-vector-ref ,vec-bind 0))
                           (y (float-vector-ref ,vec-bind 1)))
                       ,@body)))))


(define (vec3-cross l r)
  (vec3-dec (l r)
            (vec3 (- (* y0 z1) (* z0 y1))
                  (- (* z0 x1) (* x0 z1))
                  (- (* x0 y1) (* y0 x1)))))

(define (vec3-dot l r)
  (vec3-dec (l r)
            (+
              (* x0 x1)
              (* y0 y1)
              (* z0 z1))))

(define (vec3-normalize vec)
  (vec3-dec vec
            (let* ((sq-mag (+ (* x x) (* y y) (* z z)))
                   (inv-mag (/ 1 (sqrt sq-mag))))
              (vec3 (* x inv-mag)
                    (* y inv-mag)
                    (* z inv-mag)))))
(define (vec2-normalize vec)
  (vec2-dec vec
            (let* ((sq-mag (+ (* x x) (* y y)))
                   (inv-mag (/ 1 (sqrt sq-mag))))
              (vec2 (* x inv-mag)
                    (* y inv-mag)))))

; TCO for the win!
; I do bite my thumb at the Sufficiently Smart Compiler™, especially given that
; this Compiler is actually an admittedly fast but not very smart interpreter.
(define (foldl f lis)
  (cond
    ((null? lis) ())
    ((null? (cdr lis)) (car lis))
    (#t (letrec ((real-foldl (lambda (f acc lis)
                               (cond
                                 ((null? lis) ())
                                 ((null? (cdr lis)) (f acc (car lis)))
                                 (#t (real-foldl f (f acc (car lis)) (cdr lis)))))))
          (real-foldl f (car lis) (cdr lis))))))

; No TCO to be found here.
(define (foldr f lis)
  (cond
    ((null? lis) ())
    ((null? (cdr lis)) (car lis))
    (#t (f (car lis) (foldr f (cdr lis))))))

(define (vec3- v . extra)
  (if (null? extra)
    (vec3-dec v (vec3 (- x) (- y) (- z)))
    (foldl (lambda (x y)
             (vec3-dec (x y)
                       (vec3 (- x0 x1)
                             (- y0 y1)
                             (- z0 z1)))) (cons v extra))))

(define (vec3+ v . extra)
  (foldl (lambda (x y)
           (vec3-dec (x y)
                     (vec3 (+ x0 x1)
                           (+ y0 y1)
                           (+ z0 z1)))) (cons v extra)))

(define (vec3* scalar vec)
  (vec3-dec vec
            (vec3 (* x scalar)
                  (* y scalar)
                  (* z scalar))))

(define (vec2- v . extra)
  (if (null? extra)
    (vec2-dec v (vec2 (- x) (- y)))
    (foldl (lambda (x y)
             (vec2-dec (x y)
                       (vec2 (- x0 x1)
                             (- y0 y1)))) (cons v extra))))

(define (vec2+ v . extra)
  (foldl (lambda (x y)
           (vec2-dec (x y)
                     (vec2 (+ x0 x1)
                           (+ y0 y1)))) (cons v extra)))

(define (vec2* scalar vec)
  (vec2-dec vec
            (vec2 (* x scalar)
                  (* y scalar))))
