;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

 (require 'use-package)

(dolist (mode
	 '(tool-bar-mode
	   scroll-bar-mode
	   blink-cursor-mode
	   menu-bar-mode))
  (funcall mode 0))

(dolist (mode
         '(global-hl-line-mode
           global-visual-line-mode
           display-time-mode
           display-battery-mode))
 (funcall mode t))

(use-package all-the-icons
  :ensure t
  :config
  (all-the-icons-install-fonts t))

(use-package neotree
  :ensure t
  :bind ("C-x C-n" . neotree-toggle))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-major-mode-color-icon t)
      (setq doom-modeline-minor-modes nil))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t))

(use-package beacon
  :ensure t)

(beacon-mode t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode t))

(set-frame-parameter (selected-frame) 'alpha '(75 . 30))
(add-to-list 'default-frame-alist '(alpha . (75 . 30)))

(use-package helm
  :ensure t
  :bind (("C-c h" . helm-command-prefix)
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-c o" . helm-occur)
	 ("C-x b" . helm-mini))
  :config
  (require 'helm-config)
  (helm-autoresize-mode t)
  (helm-mode t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t))

(use-package helm-swoop
  :ensure t
  :bind
  ("C-s" . helm-swoop))

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t))

(use-package org
  :ensure t
  :config
  (setq org-pretty-entities t)
  (setq org-agenda-files '("~/gtd/gtd.org"))
  (setq org-refile-targets '(("gtd.org")))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)"
  "WAITING(w)" "APPT(a)" "HOMEWORK(h)" "EXAM(e)" "BILL(b)" "WORK(m)" "|"
  "DONE(d)" "CANCELLED(c)")))
  (setq org-tag-alist '(("@school" . ?s)
			    ("@home" . ?h)
			    ("email" . ?e)
			    ("phone" . ?p)
			    ("habit" . ?H)
			    ("emacs" . ?E)
			    ("wife" . ?w)
			    ("personal" . ?P)
			    ("outcome" . ?o)
			    ("750words" . ?7)))
  (setq org-capture-templates '(
                                ("t" "Task" entry
                                 (file+headline "~/gtd/gtd.org" "Tasks")
                                 "* TODO %i%?")
                                ("a" "Appointment" entry
				 (file+headline "~/gtd/tickler.org" "Appointments")
				 "* APPT %i%? %^g \n SCHEDULED: %^T")))
  (org-indent-mode t))

(use-package org-journal
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
        (quote ((:name "Appointments" :todo "APPT" :time-grid t)))))

(use-package smartparens
  :ensure t
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode t))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(75 . 50) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 79
              auto-fill-function 'do-auto-fill)
