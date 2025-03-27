#lang racket
(if (if #t #f #t)
    (if #t 1 2)
    (if #f 3 4))
