(let ([abc (read)])
  (let ([count 0])
    (begin
      (while (< count abc)
        (set! count (+ count 1))
      )
      count
    )
  )
)