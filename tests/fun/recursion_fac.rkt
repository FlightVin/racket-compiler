(define (fac [n : Integer]) : Integer
  (if (eq? n 0)
      1
      (* n (fac (- n 1)))))
(fac 5)
