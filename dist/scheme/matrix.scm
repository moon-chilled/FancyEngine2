(load "vector.scm")

(define (matx
           v0  v1  v2  v3
           v4  v5  v6  v7
           v8  v9 v10 v11
          v12 v13 v14 v15)
  (float-vector  v0  v1  v2  v3
                 v4  v5  v6  v7
                 v8  v9 v10 v11
                v12 v13 v14 v15))

(define (matx-identity)
  (matx 1 0 0 0
        0 1 0 0
        0 0 1 0
        0 0 0 1))

(define-macro (matx-dec matx . code)
              `(let (
                     ( v0 (float-vector-ref ,matx  0)) ( v1 (float-vector-ref ,matx  1)) ( v2 (float-vector-ref ,matx  2)) ( v3 (float-vector-ref ,matx  3))
                     ( v4 (float-vector-ref ,matx  4)) ( v5 (float-vector-ref ,matx  5)) ( v6 (float-vector-ref ,matx  6)) ( v7 (float-vector-ref ,matx  7))
                     ( v8 (float-vector-ref ,matx  8)) ( v9 (float-vector-ref ,matx  9)) (v10 (float-vector-ref ,matx 10)) (v11 (float-vector-ref ,matx 11))
                     (v12 (float-vector-ref ,matx 12)) (v13 (float-vector-ref ,matx 13)) (v14 (float-vector-ref ,matx 14)) (v15 (float-vector-ref ,matx 15)))

                 ,@code))

(define (matx-translate matrix trans)
  (matx-dec matrix
            (vec3-dec trans
                      (matx  v0  v1  v2 (+  v3 x)
                             v4  v5  v6 (+  v7 y)
                             v8  v9 v10 (+ v11 z)
                            v12 v13 v14 v15))))

(define (matx-perspective fov aspect near far)
  (let ((f (/ (tan (/ fov 2))))
        (d (/ (- near far))))

    (matx (/ f aspect) 0 0 0
          0 f 0 0
          0 0 (* d (+ near far)) (* 2 d far near)
          0 0 -1 0)))

(define (matx-rotation angle axis)
  (let* ((c (cos angle))
         (c- (- 1 c))
         (s (sin angle))
         (axis- (vec3-normalize axis)))
    (vec3-dec axis-
              (matx
                (+ (* x x c-) c)        (- (* x y c-) (* z s))  (+ (* x z c-) (* y s))  0

                (+ (* y x c-) (* z s))  (+ (* y y c-) c)        (- (* y z c-) (* x s))  0

                (- (* z x c-) (* y s))  (+ (* z y c-) (* x s))  (+ (* z z c-) c)        0

                0                       0                       0                       1))))

(define (matx-lookat eye target up)
  (let* ((Z (vec3-normalize (vec3- eye target)))
         (X (vec3-normalize (vec3-cross (vec3- up) Z)))
         (Y (vec3-cross Z (vec3- X))))
    (matx
      (- (.x X))  (- (.y X))  (- (.z X))     (vec3-dot X eye)
         (.x Y)      (.y Y)      (.z Y)   (- (vec3-dot Y eye))
         (.x Z)      (.y Z)      (.z Z)   (- (vec3-dot Z eye))
      0           0           0           1)))
