(let ([i 5])
  (let ([sum 0])
    (begin
      (while (> i 0)
        (begin
          (set! sum (+ sum i))
          (if (eq? i 3)
              (set! i 1)
              (set! i (- i 1)))))
       sum)))
