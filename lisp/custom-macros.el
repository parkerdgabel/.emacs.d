;;; package --- Summary
;;; Commentary:
;; My custom macros
;;; Code:

(defmacro require*
    (&rest args)
  `(mapc #'require '(,@args)))

(provide 'custom-macros)
;;; custom-macros.el ends here.
