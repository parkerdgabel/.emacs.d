;; Bootstrap straight.el package manager

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; We need to get use-package
(straight-use-package 'use-package)
;; Make use-package use straight by default for package management.
(setq straight-use-package-by-default t)

;; Flycheck for syntax checking

(use-package flycheck)

;; Helm for awesome searching
(use-package helm
  :bind
  (("M-x" . 'helm-M-x)
   ("C-x C-f" . 'helm-find-files))
  :config
  (require 'helm-config)
  (setq helm-mode-fuzzy-match t)
  (helm-autoresize-mode t)
  (setq helm-display-source-at-screen-top t)
  (setq helm-split-window-inside-p nil))

(use-package helm-swoop
  :bind ("C-s" . 'helm-swoop))

(use-package helm-company
  :after 'company)

(use-package helm-rg)

;; Company for in buffer completions
(use-package company
  :config
  (global-company-mode t)
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0))

(use-package company-quickhelp :config (company-quickhelp-mode))

;; Language Server Protocol

(use-package lsp-mode
  :config
  (defun my-lsp-mode-hook ()
    "This is my hook for lsp mode"
    (progn
      (make-local-variable 'company-backends)
      (push 'company-lsp company-backends)))
  (add-hook 'lsp-mode-hook 'my-lsp-mode-hook))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package company-lsp :commands company-lsp)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Projectile for Projects

(use-package projectile
  :config
  (projectile-mode 1))

(use-package helm-projectile)

;; (use-package objed
;;   :config (objed-mode))

;; Hydra for efficency

;; (use-package hydra
;;   :config
  
;;   (defhydra hydra-navigation (global-map "C-c s" :color pink) 
;;     "Navigation"
;;     ("s" backward-char "left" :column "Char")
;;     ("d" forward-char "right" :column "Char")
;;     ("a" backward-word "left" :column "Word")
;;     ("f" forward-word "right" :column "Word")
;;     ("e" previous-line "up" :column "Line")
;;     ("x" next-line "down" :column "Line")
;;     ("r" scroll-down-command "up screen" :column "Scroll")
;;     ("c" scroll-up-command "down screen" :column "Scroll")
;;     ("w" scroll-down-line "up line" :column "Scroll")
;;     ("z" scroll-up-line "down line" :column "Scroll")
;;     ("q" beginning-of-line "beginning" :column "Line")
;;     ("t" end-of-line "end" :column "Line")
;;     ("A" backward-paragraph "up" :column "Paragraph")
;;     ("F" forward-paragraph "down" :column "Paragraph")
;;     ("E" hydra-edit/body "edit" :column "Maps" :exit t)
;;     ("`" nil "exit" :column "Exit" :color blue))

;;   (defhydra hydra-edit (global-map "C-c e" :color pink)
;;     "Edit"
;;     ("s" delete-backward-char "left" :column "Char")
;;     ("d" delete-forward-char "right" :column "Char")
;;     ("a" backward-kill-word "left" :column "Word")
;;     ("f" kill-word "right" :column "Word")
;;     ("e" kill-line "kill line" :column "Line")
;;     ("q" beginning-of-line "beginning" :column "Line")
;;     ("t" end-of-line "end" :column "Line")
;;     ("x" kill-region "kill region" :column "Region")
;;     ("c" kill-ring-save "Save to kill-ring" :column "Region")
;;     ("w" yank "yank" :column "Region")
;;     ("A" backward-kill-paragraph "backward" :column "Paragraph")
;;     ("F" kill-paragraph "forward" :column "Paragraph")
;;     ("z" undo-tree-undo "undo" :column "Undo/Redo")
;;     ("r" undo-tree-redo "redo" :column "Undo/Redo")
;;     ("E" hydra-navigation/body "navigation" :column "Maps" :exit t)
;;     ("`" nil "exit" :column "Exit" :color blue))

;;   (defhydra hydra-files (global-map "C-c f" :color blue)
;;     "Files"
;;     ("f" helm-find-files "Find")))

;; Which key is useful to remember keybindings
(use-package which-key
  :config
  (which-key-mode t))

;; Telephone has a sweet modeline
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator telephone-line-flat)
  (setq telephone-line-primary-right-separator telephone-line-flat)
  (telephone-line-defsegment* telephone-line-xaf-fly-keys ()
    "Displays the current Xah-Fly-Keys mode"
    (when (bound-and-true-p xah-fly-keys)
      (let ((tag (if xah-fly-insert-state-q
		     "COMMAND"
		   ("INSERT"))))
	tag
	)))
  (setq telephone-line-lhs
	'((xah . (telephone-line-xah-fly-keys-segment))
	  (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (
                     telephone-line-buffer-segment))))
  (telephone-line-mode 1))

;; Lispy and Lispyville for smooth lisp editing
(use-package lispy
  :config
  (add-hook 'lisp-mode-hook 'lispy-mode))

(use-package lispyville
  :config
  (add-hook 'lispy-mode-hook 'lispyville-mode))

;; Eyebrowse for managing windows

(use-package eyebrowse
  :config
  (eyebrowse-mode t))

;; Helpful improves emacs help methods

(use-package helpful
  :bind
  (("C-h f" . 'helpful-callable)
   ("C-h v" . 'helpful-variable)
   ("C-h k" . 'helpful-key)
   ("C-c C-d" . 'helpful-at-point)
   ("C-h F" . 'helpful-function)
   ("C-h C" . 'helpful-command)))

;; El-patch allows for future-proofing Emacs

(use-package el-patch)

;; I need my sinppets

(use-package yasnippet
  :config (yas-global-mode t)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package yasnippet-snippets)

;; Doc Forms

(defun my-text-mode-hook
    ()
  (progn
    (auto-fill-mode t)
    (setq fill-column 80)
    (flyspell-mode t)
    (make-local-variable 'company-backends)
    (push 'company-ispell company-backends)))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(use-package adoc-mode
  :config
  (add-hook 'adoc-mode-hook 'my-text-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook 'my-text-mode-hook))

(use-package org
  :config
  (setq org-hide-leading-stars t)
  (setq org-directory "/Users/parkergabel/Dropbox/.org")
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-reload)
  (add-to-list 'org-src-lang-modes '("scala" . scala))
  (setq org-publish-project-alist
	'(("A Compendium of Computational Objects"
	   :base-directory "~/Dropbox/Books/CompendiumOfComputationalObjects"
	   :publishing-directory "~/Dropbox/Books/CompendiumOfComputationalObjects/public_html"
	   :publishing-function org-html-publish-to-html
	   :with-author t
	   :qith-toc t
	   :htmlized-source t
	   ))))

(use-package deft
  :config
  (setq deft-directory "/Users/parkergabel/Dropbox/.deft")
  (setq deft-default-extension "org"))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))



;; Themes

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'colors))

(use-package doom-themes)
;; Writeroom for focused writing

(use-package writeroom-mode)
;; Beacon to keep track of the cursor

(use-package beacon
  :config
  (beacon-mode t))

;; Rainbow Parens for lispy goodness
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight Surrounding Parens
(use-package highlight-parentheses
  :config
  (highlight-parentheses-mode t))

;; Languages

;; Java

(use-package lsp-java :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; Ruby

(use-package ruby-end)

(use-package robe)

(add-hook 'ruby-mode-hook 'robe-mode)

(defun my-ruby-hook
    ()
  (make-local-variable 'company-backends)
  (push 'company-robe company-backends))

(add-hook 'ruby-mode-hook 'my-ruby-hook)

(require 'ob-ruby)

;; Cider for clojure

(use-package cider)
(require 'ob-clojure)

(add-hook 'clojure-mode 'lispy-mode)
(setq org-babel-clojure-backend 'cider)
;; Emacs-lisp

(use-package eros
  :config
  (eros-mode t))

(add-hook 'emacs-lisp-mode-hook 'lispy-mode)

;; Ledger Accounting
(use-package ledger-mode)

;;; Basic configuration
(use-package hledger-mode)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "/Users/parkergabel/Dropbox/Accounting/all.journal")

;; Rust

(use-package rustic)

;; Golang

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp)

  (defun my-go-mode-hook ()
    (progn
      (make-local-variable 'company-backends)
      (add-to-list 'company-backends 'company-go)))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go)

;; Scala

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package ob-ammonite)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(provide 'packages)
