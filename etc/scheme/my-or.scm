(define-syntax or
  (syntax-rules (a b)
    ((or) #f)
    ((or test) test)
    ((or a test1 test2 ...) (let ((x test1)) (if x x (or test2 ...))))))