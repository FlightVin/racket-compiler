(let ([x 1])
  (begin
    (let ([i 3])
      (while (> i 0)
        (begin
          (set! x (+ x 1))
          (set! i (- i 1)))))
    x))
