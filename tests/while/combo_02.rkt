(let ([outer 2])
  (let ([total 0])
    (begin
      (while (> outer 0)
        (begin
          (let ([inner 3])
            (while (> inner 0)
              (begin
                (set! total (+ total 1))
                (set! inner (- inner 1)))))
          (set! outer (- outer 1))))
      total)))
