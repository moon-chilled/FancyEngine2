(define (vec3 x y z)
  (float-vector x y z))

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

(define (vec3-cross l r)
  (vec3-dec l
            (vec3 (- (* y (get-z r)) (* z (get-y r)))
                  (- (* z (get-x r)) (* x (get-z r)))
                  (- (* x (get-y r)) (* y (get-x r))))))

(define (vec3-dot l r)
  (vec3-dec l
            (vec3 (* x (get-x r))
                  (* y (get-y r))
                  (* z (get-z r)))))

(define (vec3-normalize vec)
  (vec3-dec vec
            (let* ((sq-mag (+ (* x x) (* y y) (* z z)))
                   (inv-mag (/ 1 (sqrt sq-mag))))
              (vec3 (* x inv-mag)
                    (* y inv-mag)
                    (* z inv-mag)))))
