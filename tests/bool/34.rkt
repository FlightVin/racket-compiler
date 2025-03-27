(let ([flag (if #t #f #t)])
  (let ([result (if flag
                    (if (and flag (not flag))
                        1
                        2)
                    (if (or flag (not flag))
                        3
                        4))])
    result))
