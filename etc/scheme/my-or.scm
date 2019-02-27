(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...) (let ((x test1)) (if x x (or test2 ...))))))