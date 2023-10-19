(require 'package) ;; Emacs builtin

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(setq package-archives
'(
   ("org" . "https://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; initialize built-in package management
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq straight-use-package-by-default t)

;; now you can
;; (use-package pkgname) etc as per
;; use-package example docs
(use-package diminish)

(use-package company
  :ensure t
  :delight company-mode
  :demand t
  :straight t
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
	company-show-quick-access t)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p". company-select-previous)
	      ("C-'" . company-abort))
  :config
  (global-company-mode))

;; (use-package avy
;;   :bind
;;   ;; (("M-j l" . avy-goto-word-crt-line)
;;   ;;  ("M-j j" . avy-goto-char-timer))
;;   )

(use-package gcmh
  :diminish t
  :straight t
  :config
  (gcmh-mode))

;; (use-package objed
;;   :diminish t
;;   :straight t
;;   :bind
;;   (("s-SPC" . objed-activate))
;;   :hook
;;   (text-mode . objed-mode))

(tool-bar-mode -1)
  
(scroll-bar-mode -1)

(menu-bar-mode -1)

(electric-pair-mode t)

(use-package solaire-mode
  :diminish t
  :config
  (solaire-global-mode +1))

(use-package doom-themes
  :ensure t
  :straight t
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-solarized-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  (use-package doom-themes-ext-treemacs
    :init
    (setq doom-themes-treemacs-theme "doom-colors")) ; use the colorful treemacs theme)
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :straight t
  :init
  (setq doom-modeline-minor-modes t
        doom-modeline-vcs-max-length 20)
  (setq doom-modeline-enable-word-count t)
  :config
  (doom-modeline-mode 1))

(use-package ef-themes
  :straight t)

(use-package palimpsest
  :diminish t
  :straight t)

(use-package langtool
  :straight t)

(use-package rainbow-delimiters
  :ensure t
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )


(use-package org
  :ensure t
  :straight t
  :bind (:map org-mode-map
	      ("C-M-e" . org-forward-paragraph)
	      ("C-M-a" . org-backward-paragraph))
  :config
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;;
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Separate drawers for clocking and logs
  (setq org-drawers '("PROPERTIES" "LOGBOOK"))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  '(setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  :hook
  (org-mode . abbrev-mode))

(use-package org-tracktable
  :straight t
  :init
  (setq org-tracktable-daily-goal 500))

(use-package org-sticky-header
  :straight t
  :hook
  (org-mode . org-sticky-header-mode))

;; (use-package org-plus-contrib
;;   :after org
;;   :ensure t)


(use-package projectile
  :ensure t
  :straight t
  :bind-keymap ("s-p" . projectile-command-map)
  :init
  (setq projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  :config
  (projectile-mode +1))

(use-package time
  :ensure nil
  :straight t
  :init
  (setq display-time-day-and-date t
        display-time-24hr-format t
        display-time-interval 10)
  :config
  (display-time))

(use-package vertico-posframe
  :after vertico)

(use-package dash)

(use-package ns-win
  :ensure nil
  :straight nil
  :init
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;; Enable vertico
(use-package vertico
  :ensure t
  :straight t
  :config
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 40)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package vertico-buffer-mode
  :straight nil
  :hook
  (vertico-mode . vertico-buffer-mode)
  :after 'vertico)


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :straight t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :ensure t
  :straight t
  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-org-heading)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package night-owl-theme
  :straight t)

(use-package embark
  :ensure t
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-company
  :init
  (define-key company-mode-map [remap completion-at-point] #'consult-company))

(use-package abbrev
  :straight nil
  :config
  (abbrev-mode)
  :hook
  (text-mode . abbrev-mode))

(use-package crux
  :straight t)

(use-package company-wordfreq
  :straight t
  :hook
  (text-mode . (lambda ()
		 (setq-local company-backends '((company-wordfreq company-ispell :seperate company-dabbrev company-abbrev)))
		 (setq-local company-transformers nil))))

(use-package eros
  :straight t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package adoc-mode
  :straight t)

(use-package which-key
  :straight t
  :init
  (which-key-mode))

(use-package magit
  :straight t)

(use-package lispy
  :straight t
  :hook
  (emacs-lisp-mode . lispy-mode))

(use-package git-gutter
  :diminish t
  :straight t
  :init
  (global-git-gutter-mode t))

(use-package poet-theme
  :straight t)

(use-package org-modern
  :straight t
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  :custom
  (org-modern-table nil))

(use-package org-appear)

(use-package eshell
  :straight nil
  :hook
  (eshell-mode . (lambda () (setenv "TERM" "xterm-256color"))))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode t))

(use-package company-shell
  :after 'company
  :hook
  (eshell-mode . (lambda () (setq-local company-backends (--> '(company-shell company-shell-env) (push it company-backends))))))

(use-package pdf-tools)

(use-package nov)

(use-package ox-pandoc
  :after org)


(use-package centered-cursor-mode
  :straight t
  :demand
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  ;; (global-centered-cursor-mode)
  )

(use-package olivetti
  :straight t
  :config
  (setq olivetti-style 'fancy)
  (setq olivetti-body-width 140))

(use-package beacon
  :straight t
  :config
  (beacon-mode))

(use-package mixed-pitch
  :straight t
  :hook
  (text-mode . mixed-pitch-mode))

(use-package flyspell
  :config
  (setq ispell-program-name "hunspell"
        ispell-default-dictionary "en_US")
  (unbind-key "C-." flyspell-mode-map)
  :hook (text-mode . flyspell-mode)
  :bind (("M-<f7>" . flyspell-buffer)
         ("<f7>" . flyspell-word)
         ("C-;" . flyspell-auto-correct-previous-word)))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
	    source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  )

(add-to-list 'flycheck-checkers 'vale 'append))

(use-package consult-flyspell
  :straight t
  :config
  )

(use-package company-posframe
  :straight t
  :config
  (company-posframe-mode 1))

(use-package company-box
  ;; :hook (company-mode . company-box-mode)
  )

(use-package hydra
  :config
  (defhydra hydra-org (org-mode-map "C-c C-/" :colums 4 :pre (setq which-key-inhibit t)
				    :post (setq which-key-inhibit nil))
    "org-mode"
    ("j" next-line "next line")
    ("k" previous-line "previous line")
    ("h" backward-char "backward char")
    ("l" forward-char "forward char")
    ("f" forward-word "forward word")

    ("a" backward-word "backward word")
    ("d" forward-sentence "forward sentence")
    ("s" backward-sentence "backward sentence")
    ("v" org-forward-paragraph "forward paragraph")
    ("c" org-backward-paragraph "backward paragraph")

    ("J" avy-goto-line "goto line")
    ("F" avy-goto-word-crt-line "goto word in line")
    ("n" org-next-visible-heading "Next heading")
    ("p" org-previous-visible-heading "Previous heading")

    ("I" org-clock-in "Clock in")
    ("O" org-clock-out "Clock out")
    ("D" kill-word "Kill word")
    ("K" kill-sentece "kill sentence")
    ("/" undo "undo")
    ("q" nil "cancel" :color blue)))

(add-hook 'text-mode-hook 'visual-line-mode)

(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package custom-avy
  :straight nil)

(native-compile-async "~/.emacs.d/straight/build/org" t t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/fiction/post-apocalypse-patrol/outline.org"))
 '(package-selected-packages
   '(magit which-key adoc-mode company-wordfreq consult-company embark-consult embark marginalia orderless vertico rainbow-delimiters projectile doom-themes doom-modeline company))
 '(vertico-buffer-display-action
   '(display-buffer-in-side-window
     (side . left)
     (window-width . 0.3))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
