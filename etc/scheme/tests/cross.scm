(define-syntax cross
  (syntax-rules (a b)
    ((_ a (t ...) ...) (t ... ...))
    ((_ b (t ...) ...) ((t ...) ...))
    ))