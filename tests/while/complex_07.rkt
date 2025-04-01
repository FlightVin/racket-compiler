(let ([a 0])
 (let ([b 0])
  (begin
   (set! a 5)
   (set! b (+ a 10))
   (set! a (+ b 2))
   a)))
