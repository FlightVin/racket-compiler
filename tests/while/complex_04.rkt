(let ([x 1])
 (let ([y 2])
  (let ([z 3])
   (begin
    (let ([x 10])
     (begin
      (set! y (+ x 5))
      (let ([x 100])
       (set! z (+ x y)))))
    (+ x (+ y z))))))
