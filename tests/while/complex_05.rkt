(let ([limit (read)])
 (let ([val 0])
  (let ([i 0])
   (begin
    (while (and (< i limit) (not (eq? val (- 0 5))))
     (begin
      (set! val (read))
      (set! i (+ i 1))))
    val))))
