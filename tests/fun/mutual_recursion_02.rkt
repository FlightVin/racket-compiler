(define (is-even? [n : Integer]) : Boolean
  (if (eq? n 0)
      #t
      (is-odd? (- n 1))))
(define (is-odd? [n : Integer]) : Boolean
  (if (eq? n 0)
      #f
      (is-even? (- n 1))))
(is-odd? 5)
