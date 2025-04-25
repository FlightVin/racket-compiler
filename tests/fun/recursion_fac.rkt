(define (fac [n : Integer]) : Integer
  (if (eq? n 1)
      1
      (+ n (fac (- n 1)))))
(fac 5)
