(let ([x (read)])
    (begin
        (while (read)
            (begin
                (set! x (+ x 1))
            )
        )
    x)
)