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

(set-fringe-mode nil)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind ("C-x C-n" . neotree-toggle))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
      :defer t
      :ensure t
      :config
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-major-mode-color-icon t)
      (setq doom-modeline-minor-modes nil))

(use-package powerline
  :ensure t)

(use-package micgoline
  :ensure t
  :hook (after-init-hook . micgoline-load-theme))

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

(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode)
  (diminish 'auto-fill-mode))

(use-package beacon
  :diminish beacon-mode
  :ensure t)

(beacon-mode t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))

(use-package dimmer
  :ensure t
  :diminish dimmer-mode
  :config
  (dimmer-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-c o" . helm-occur)
	 ("C-x C-b" . helm-mini))
  :config
  (require 'helm-config)
  (helm-autoresize-mode t)
  (helm-mode t)
  (setq helm-split-window-in-side-p t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind
  ("C-s" . helm-swoop))

(use-package helm-org-rifle
  :ensure t)

(use-package helm-projectile
  :after projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t))

(use-package company-quickhelp
  :ensure t
  :diminish company-quickhelp-mode
  :config
  (company-quickhelp-mode)
  (define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin))

(use-package org
  :ensure t
  :bind
  (("C-c C-a" . org-agenda)
   ("C-c C-u" . org-up-element)
   ("C-c C-d" . org-down-element))
  :config
  (setq org-pretty-entities t)
  (setq org-agenda-files '("~/Dropbox/gtd/gtd.org"))
  (setq org-refile-targets '(("~/Dropbox/gtd/gtd.org" :maxlevel . 9)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)"
  "WAITING(w)" "APPT(a)" "HOMEWORK(h)" "EXAM(e)" "BILL(b)" "WORK(m)" "|"
  "DONE(d)" "CANCELLED(c)")))
  (setq org-tag-alist '(("@school" . ?s)
			    ("@home" . ?h)
			    ("email" . ?e)
			    ("phone" . ?p)
                        ("finance" .?f)
			    ("habit" . ?H)
			    ("emacs" . ?E)
			    ("wife" . ?w)
			    ("personal" . ?P)
			    ("outcome" . ?o)
			    ("750words" . ?7)))
  (setq org-capture-templates '(("t" "Task" entry (file+headline "~/Dropbox/gtd/gtd.org" "Tasks") "* TODO %i%?"))) 
  (setq org-agenda-span 'day))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Dropbox/Journal"))

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
        (quote ((:name "Appointments" :todo "APPT")
                (:name "Homework" :todo "HOMEWORK")
                (:name "Exams" :todo "EXAM")
                (:name "Projects" :todo "PROJECT")
                (:name "Next Actions" :todo "NEXT" )
                (:name "Waiting" :todo "WAITING")
                (:name "Financial" :tag "finance")))))

(use-package org-bullets
  :ensure t)

(defun org-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (push 'company-dabbrev company-backends)
  (push 'company-ispell company-backends)
  (setq fill-column 80)
  (auto-fill-mode t)
  (flyspell-mode t)
  (set-fringe-mode nil)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (org-bullets-mode t)
  (org-indent-mode t))

(add-hook 'org-mode-hook 'org-mode-hook-setup)

(use-package hy-mode
  :ensure t
  :init
  (defun hy-mode-hook-setup ()
    (make-local-variable (quote company-backends))
    (add-to-list (quote company-backends) (quote company-hy))
    (run-jedhy))
  (add-hook (quote hy-mode-hook) (quote hy-mode-hook-setup))
  (add-hook (quote inferior-hy-mode-hook) (quote hy-mode-hook-setup)))

(use-package writeroom-mode
  :ensure t)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
   :bind
  ("C-`" . eshell-toggle))

(use-package flycheck
  :ensure t
  :diminish global-flycheck-mode flycheck-mode flyspell-mode
  :config
  (global-flycheck-mode t))

(use-package smartparens
  :ensure t
  :diminish smartparens-global-strict-mode smartparens-mode
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-strict-mode t))

(use-package pdf-tools
  :ensure t
  :config
  (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")
  (add-hook 'after-init-hook 'pdf-tools-install)
  (setq pdf-annot-activate-created-annotations t))

(use-package w3m
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed))
  :config
  (setq elfeed-feeds
  '()))

(use-package god-mode
  :ensure t
  :diminish god-mode
  :bind (("<escape>" . god-mode-all))
  :config
  (defun my-update-cursor ()
     (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

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
