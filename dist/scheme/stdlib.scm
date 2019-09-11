(load "vector.scm")
(load "matrix.scm")

(define PI 3.14159265358979323846264)

(define (clamp val min max)
  (cond
    ((<= val min) min)
    ((>= val max) max)
    (#t val)))
