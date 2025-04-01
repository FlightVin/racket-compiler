(let ([sum 0])
  (let ([i 3])
    (begin
      (while (> i 0)
        (begin
          (set! sum (+ sum (read)))
          (set! i (- i 1))))
      sum)))
