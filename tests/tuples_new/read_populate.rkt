(let ([v (vector 0 0)])
    (begin
        (vector-set! v 0 (read))
        (vector-set! v 1 (read))
        (vector-ref v 1)
    )
)
