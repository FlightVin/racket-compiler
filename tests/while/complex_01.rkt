(let ([i 10])
 (let ([s 0])
  (begin
   (while (>= i 0)
    (begin
     (set! s (+ s i))
     (if (eq? (read) 1)
      (set! i (- i 2))
      (set! i (- i 1)))))
   s)))
