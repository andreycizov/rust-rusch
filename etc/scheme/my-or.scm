(syntax-rules ()
  ((my-or) #f)
  ((my-or e) e)
  ((my-or e1 e2 ...)
   (let ((temp e1)) (if temptemp (my-or e2 ...))))
  )