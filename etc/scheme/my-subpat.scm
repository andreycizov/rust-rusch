(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or t ...) (t ... ...))
    ((or (test1 test2 ...) (a b ...) ...) (let ((x test1)) (if x x (or test2 ...))))
    ))