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

(use-package helm
  :ensure t
  :bind (("C-c h" . helm-command-prefix)
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
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t))

(use-package org
  :ensure t
  :bind
  (("C-c a" . org-agenda)
   ("C-c C-x n" . org-capture)
   ("C-c u" . org-up-element)
   ("C-c d" . org-down-element))
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
  (setq org-capture-templates '(
                                ("t" "Task" entry
                                 (file+headline "~/Dropbox/gtd/gtd.org" "Tasks")
                                 "* TODO %i%?")
                                ("a" "Appointment" entry
				 (file+headline "~/Dropbox/gtd/gtd.org" "Appointments")
				 "* APPT %i%? %^g \n SCHEDULED: %^T")))
  (setq org-agenda-span 'day))

(use-package org-journal
  :ensure t)

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

(use-package org-noter
    :after org
    :ensure t
    :config (setq org-noter-default-notes-file-names '("notes.org")
                  org-noter-notes-search-path '("~/Dropbox/Books")
                  org-noter-separate-notes-from-heading t))

(defun org-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-ispell)
  (org-bullets-mode t)
  (org-indent-mode t))

(add-hook 'org-mode-hook 'org-mode-hook-setup)

(use-package writeroom-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package smartparens
  :ensure t
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-<backspace>" . sp-kill-sexp))
  :config
  (require 'smartparens-config)
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
  '(("https://rss.nytimes.com/services/xml/rss/nyt/Business.xml" nyt business)
    ("https://feeds.a.dj.com/rss/RSSMarketsMain.xml" wsj markets)
    ("http://rss.cnn.com/rss/money_pf.rss" cnn finance)
    ("https://hbswk.hbs.edu/stories-rss.aspx" hbs business)
    ("https://www.feedspot.com/infiniterss.php?q=site:http%3A%2F%2Fwww.thepennyhoarder.com%2Ffeed"
  ph finance))))

(use-package hledger-mode
  :ensure t
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
   :init
   )

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
