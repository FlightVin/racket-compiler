(let ([x 10])
 (let ([y 0])
  (let ([i 2])
   (begin
    (while (> i 0)
     (begin
      (set! y (+ x 1))
      (set! x (- x 1))
      (set! i (- i 1))))
    x))))
