(let ([x (read)])
    (begin
        (while (eq? 2 (read))
            (begin
                (set! x (+ x 1))
            )
        )
    x)
)