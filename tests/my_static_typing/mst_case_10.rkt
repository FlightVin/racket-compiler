(let ([a (read)]) (let ([b 0]) (begin (set! b (+ a 1)) (if (> b 10) a (- 0 a)))))
