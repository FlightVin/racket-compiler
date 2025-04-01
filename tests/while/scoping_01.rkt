(let ([i 3])
 (begin
  (while (> i 0)
   (let ([inner_var (+ i 10)])
    (begin
     (set! i (- i 1))
     (void))))
  (set! inner_var 100)))