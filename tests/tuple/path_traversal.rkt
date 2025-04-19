(let ([leaf1 (vector 1 2)])
  (let ([leaf2 (vector 3 4)])
    (let ([leaf3 (vector 5 6)])
      (let ([branch1 (vector leaf1 leaf2)])
        (let ([branch2 (vector leaf2 leaf3)])
          (let ([root (vector branch1 branch2)])
            (begin
              (vector-set! (vector-ref (vector-ref root 0) 1) 0 42)
              (vector-ref (vector-ref (vector-ref root 1) 0) 0))))))))
