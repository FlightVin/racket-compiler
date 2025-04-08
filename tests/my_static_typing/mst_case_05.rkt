(let ([x (read)]) (let ([y (eq? x 5)]) (if y (let ([x #f]) (not x)) (< x 0))))
