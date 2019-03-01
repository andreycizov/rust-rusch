#| independent of the call site, a definition clearly defines how to op must be called. |#

(define
 main
 (lambda
  (x . x)
  ("abc"))
)

#| how do we return a closure (?). rather - how do we encode a closure |#


#| a closure is a lambda with more than 0 free variables. |#

(define
  closure_234656
  (closure
    (a b c) (e f g)
    (body 5)))

(define
  closure_23456
  (closure
    (i y z) (e f h)
    (3)))

(define
  zzz_hugely
 (lambda (a b c)
  ())
)

#| nanop is a call to a direct microcode instruction |#
(define musings (nanop ()))

