;; My configurations for Emacs

(setq make-backup-files nil)

(auto-save-mode nil)

(add-hook 'prog-mode-hook (lambda () (auto-save-mode 0)))

(add-hook 'text-mode-hook (lambda () (auto-save-mode 0)))

(setq gc-cons-threshold 20000000)

(fset 'yes-or-no-p 'y-or-n-p)

(display-time-mode t)

(tool-bar-mode 0)

(scroll-bar-mode -1)

(tooltip-mode 0)

(menu-bar-mode 0)

(global-prettify-symbols-mode t)

(global-linum-mode t)

(electric-pair-mode t)

(provide 'my-config)
