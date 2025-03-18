(let ([x (let ([y 10]) y)])
 (let ([z (+ x 5)])
   (+ x z)))