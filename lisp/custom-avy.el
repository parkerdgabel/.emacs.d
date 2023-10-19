(defun avy-org-same-level (&optional all)
  "Go to any org heading of the same level as the current one.

By default, choices are limited to headings under common
subheading, but if called with a prefix argument, will be
buffer-global."
  (interactive "P")
  (let ((org-level (org-current-level)))
    (avy--generic-jump
     (format "^%s "
	     (regexp-quote
	      (make-string org-level ?*)))
     nil
     'pre
     (unless (or all (= org-level 1))
       (save-excursion
	 (outline-up-heading 1)
	 (point)))
     (unless (or all (= org-level 1))
       (save-excursion
	 (outline-up-heading 1)
	 (org-end-of-subtree))))))

(defun avy-org-parent-level (&optional all)
  "Go to any org heading one level above the current one.

By default, choices are limited to headings under common
subheading, but if called with a prefix argument, will be
buffer-global."
  (interactive "P")
  (let ((org-level (org-current-level)))
    (if (= org-level 1)
        (message "Already at top level.")
      (avy--generic-jump
       (format "^%s "
               (regexp-quote
                (make-string (- org-level 1) ?*)))
       nil
       'pre
       (unless (or all (= org-level 2))
         (save-excursion
           (outline-up-heading 2)
           (point)))
       (unless (or all (= org-level 2))
         (save-excursion
           (outline-up-heading 2)
           (org-end-of-subtree)))))))

(defun avy-org-child-level (&optional all)
  "Go to any org heading one level below the current one.

By default, choices are limited to headings under common
subheading, but if called with a prefix argument, will be
buffer-global."
  (interactive "P")
  (if (save-excursion (org-goto-first-child))
      (let ((org-level (org-current-level)))
        (avy--generic-jump
         (format "^%s "
                 (regexp-quote
                  (make-string (+ org-level 1) ?*)))
         nil
         'pre
         (unless all
           (save-excursion
             (ignore-errors
               (outline-up-heading 0))
             (point)))
         (unless all
           (save-excursion
             (ignore-errors
               (outline-up-heading 0))
             (org-end-of-subtree)))))
    (message "Heading has no children.")))

(defun avy-org-goto-level (&optional num)
  "Prompt for an org level to go to, defaulting to the current one."
  (interactive (list
		(read-number "Select heading level: " (org-current-level))))
  (avy--generic-jump
   (format "^%s " (regexp-quote (make-string num ?*)))
   nil
   'pre))

(defun avy-goto-word-crt-line ()
  "Jump to a word start on the current line only."
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))


;; (defun avy-goto-word-in-sentence ()
;;   (interactive)
;;   (let ((beg (save-excursion (unless ()))))
;;     (avy-with avy-goto-word-0
;;       (avy-goto-word-0 nil beg end))))

(provide 'custom-avy)