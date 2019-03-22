(define-syntax or
  (syntax-rules (a b)
    ((or) '#f)
    ((or test) 'test)
    ((or a (t ...) ...) '(t ... ...))
    ((or b (t ...) ...) '((t ...) ...))
    ))
