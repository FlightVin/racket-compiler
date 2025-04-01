(let ([sum 0])
 (let ([i 3])
  (begin
   (while (> i 0)
    (let ([inner_val (+ i 100)])
     (begin
      (set! sum (+ sum inner_val))
      (set! i (- i 1)))))
   sum)))
