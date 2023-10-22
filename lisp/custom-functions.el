;; -*- lexical-binding: t -*-
;;; package --- Summary
;;; Commentary:
;;; These are my custom functions
;;; Code:

(defun my-insert-newline-below
    ()
  "Opens a newline below the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(provide 'custom-functions)
;;; custom-functions.el ends here.
