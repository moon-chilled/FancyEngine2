(define (vec2 x y)
  (float-vector x y))

(define (vec3 x y z)
  (float-vector x y z))

; Generate and (n)ormalize a vector
(define (vec3n x y z)
  (vec3-normalize (float-vector x y z)))
(define (vec2n x y z)
  (vec2-normalize (float-vector x y)))

(define (get-x vec) (float-vector-ref vec 0))
(define (get-r vec) (float-vector-ref vec 0))
(define (get-y vec) (float-vector-ref vec 1))
(define (get-g vec) (float-vector-ref vec 1))
(define (get-z vec) (float-vector-ref vec 2))
(define (get-b vec) (float-vector-ref vec 2))

(define-macro (vec3-dec vec . code)
              `(let ((x (float-vector-ref ,vec 0))
                     (y (float-vector-ref ,vec 1))
                     (z (float-vector-ref ,vec 2)))
                 ,(cons `begin code)))

(define-macro (vec2-dec vec . code)
              `(let ((x (float-vector-ref ,vec 0))
                     (y (float-vector-ref ,vec 1)))
                 ,(cons `begin code)))


(define (vec3-cross l r)
  (vec3-dec l
            (vec3 (- (* y (get-z r)) (* z (get-y r)))
                  (- (* z (get-x r)) (* x (get-z r)))
                  (- (* x (get-y r)) (* y (get-x r))))))

(define (vec3-dot l r)
  (+
    (* (get-x l) (get-x r))
    (* (get-y l) (get-y r))
    (* (get-z l) (get-z r))))

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

(define (vec3- l . extra)
  (if (null? extra)

      (vec3- (vec3 0 0 0) l)

      (vec3-dec l (let ((ret (vec3
                               (- x (get-x (car extra)))
                               (- y (get-y (car extra)))
                               (- z (get-z (car extra))))))
                    (if (null? (cdr extra))
                        ret
                        (apply vec3- (cons ret (cdr extra))))))))

(define (vec3+ l . extra)
  (if (null? extra)

      l

      (vec3-dec l (let ((ret (vec3
                               (+ x (get-x (car extra)))
                               (+ y (get-y (car extra)))
                               (+ z (get-z (car extra))))))
                    (if (null? (cdr extra))
                        ret
                        (apply vec3+ (cons ret (cdr extra))))))))
(define (vec3* scalar vec)
  (vec3-dec vec
            (vec3 (* x scalar)
                  (* y scalar)
                  (* z scalar))))

(define (vec2- l . extra)
  (if (null? extra)

      (vec2- (vec2 0 0 0) l)

      (vec2-dec l (let ((ret (vec2
                               (- x (get-x (car extra)))
                               (- y (get-y (car extra))))))
                    (if (null? (cdr extra))
                        ret
                        (apply vec2- (cons ret (cdr extra))))))))

(define (vec2+ l . extra)
  (if (null? extra)

      l

      (vec2-dec l (let ((ret (vec2
                               (+ x (get-x (car extra)))
                               (+ y (get-y (car extra))))))
                    (if (null? (cdr extra))
                        ret
                        (apply vec2+ (cons ret (cdr extra))))))))
(define (vec2* scalar vec)
  (vec2-dec vec
            (vec2 (* x scalar)
                  (* y scalar))))
