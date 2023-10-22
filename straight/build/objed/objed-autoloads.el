;;; objed-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from objed.el

(autoload 'objed-activate "objed" "\
Activate objed.

When called non interactively activate with object OBJ which
defaults to char object. Otherwise uses associated
`objed-cmd-alist' for `last-command' as initial object. Falls
back to `objed-initial-object' if no match found.

(fn &optional OBJ)" t)
(autoload 'objed-activate-object "objed" "\
Query for object and activate with it." t)
(autoload 'objed-beg-of-object-at-point "objed" "\
Activate and move to beginning of object at point.

On repeat or at boundary move to previous." t)
(autoload 'objed-end-of-object-at-point "objed" "\
Activate and move to end of object at point.

On repeat or at boundary move to next." t)
(autoload 'objed-until-beg-of-object-at-point "objed" "\
Move to beginning of object at point and active text moved over." t)
(autoload 'objed-until-end-of-object-at-point "objed" "\
Move to end of object at point and active text moved over." t)
(autoload 'objed-first-identifier "objed" "\
Move to first instance of identifier at point." t)
(autoload 'objed-last-identifier "objed" "\
Move to last instance of identifier at point." t)
(autoload 'objed-next-identifier "objed" "\
Activate object with identifier at point." t)
(autoload 'objed-prev-identifier "objed" "\
Activate object with identifier at point." t)
(defvar objed-mode nil "\
Non-nil if Objed mode is enabled.
See the `objed-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `objed-mode'.")
(custom-autoload 'objed-mode "objed" nil)
(autoload 'objed-mode "objed" "\
Enable objeds modal editing features after certain commands.

With a prefix argument ARG, enable Objed mode if ARG is positive,
and disable it otherwise. If called from Lisp, enable the mode if
ARG is omitted or nil.

Objed mode is a global minor mode. When enabled, any command
configured in `objed-cmd-alist' will activate modal navigation
and editing features on text objects. Available commands,
operations and objects can be found in `objed-map',
`objed-op-map' and `objed-object-map'.

To define your own text objects and editing operations see
`objed-define-object' and `objed-define-op'.

(fn &optional ARG)" t)
(register-definition-prefixes "objed" '("objed-"))


;;; Generated autoloads from objed-objects.el

(register-definition-prefixes "objed-objects" '("objed-"))

;;; End of scraped data

(provide 'objed-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; objed-autoloads.el ends here