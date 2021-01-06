(defmacro my-company-local
    (mode)
  '(progn
     (make-local-variable 'company-backends)
     (push (format "company-%s" mode) 'company-backends))
