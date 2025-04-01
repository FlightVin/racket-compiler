(let ([flag (read)])
 (let ([counter 0])
  (if (> flag 0)
   (begin
    (while (< counter flag)
     (set! counter (+ counter 1)))
    counter)
   (let ([x 5])
    (begin
     (while (> x 0)
      (set! x (- x 1)))
     (- 0 1))))))