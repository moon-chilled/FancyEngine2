(define (normalize-coord coord)
  (let ((rx (.x (get-renderdims)))
        (ry (.y (get-renderdims))))
    (vec2-dec coord
              (vec2
                (* 2 (/ (- x (* .5 rx)) rx))
                (* 2 (/ (- y (* .5 ry)) ry))))))

(define (draw-tex tex loc)
  (draw-tex-ndc tex
                (normalize-coord loc)))

(define (draw-text fnt text loc)                                                                                                       
  (draw-text-ndc fnt text (normalize-coord loc)))
