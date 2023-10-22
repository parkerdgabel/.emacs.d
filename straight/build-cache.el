
:tanat

"30.0.50"

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2023-10-22 10:54:34" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2023-10-22 10:54:34" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2023-10-21 20:42:36" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "nongnu-elpa" ("2023-10-21 20:42:36" nil (:type git :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git" :depth (full single-branch) :local-repo "nongnu-elpa" :build nil :package "nongnu-elpa")) "el-get" ("2023-10-21 20:42:36" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2023-10-21 20:42:36" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2023-10-11 21:04:16" ("emacs") (:type git :host github :repo "radian-software/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "diminish" ("2023-10-11 21:04:16" ("emacs") (:type git :flavor melpa :host github :repo "myrjola/diminish.el" :package "diminish" :local-repo "diminish.el")) "company" ("2023-10-17 20:56:08" ("emacs") (:type git :flavor melpa :files (:defaults "icons" ("images/small" "doc/images/small/*.png") "company-pkg.el") :host github :repo "company-mode/company-mode" :package "company" :local-repo "company-mode")) "gcmh" ("2023-10-11 21:04:16" ("emacs") (:type git :flavor melpa :host gitlab :repo "koral/gcmh" :package "gcmh" :local-repo "gcmh")) "objed" ("2023-10-11 10:07:58" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "clemera/objed" :package "objed" :local-repo "objed")) "solaire-mode" ("2023-10-11 21:04:16" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "hlissner/emacs-solaire-mode" :package "solaire-mode" :local-repo "emacs-solaire-mode")) "doom-themes" ("2023-10-11 21:04:16" ("emacs" "cl-lib") (:type git :flavor melpa :files (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el" "doom-themes-pkg.el") :host github :repo "doomemacs/themes" :package "doom-themes" :local-repo "themes")) "doom-modeline" ("2023-10-11 21:04:16" ("emacs" "compat" "nerd-icons" "shrink-path") (:type git :flavor melpa :host github :repo "seagle0128/doom-modeline" :package "doom-modeline" :local-repo "doom-modeline")) "compat" ("2023-10-22 10:54:36" ("emacs" "seq") (:type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git")) :package "compat" :local-repo "compat")) "nerd-icons" ("2023-10-11 21:04:16" ("emacs") (:type git :flavor melpa :files (:defaults "data" "nerd-icons-pkg.el") :host github :repo "rainstormstudio/nerd-icons.el" :package "nerd-icons" :local-repo "nerd-icons.el")) "shrink-path" ("2023-10-11 21:04:16" ("emacs" "s" "dash" "f") (:type git :flavor melpa :host gitlab :repo "bennya/shrink-path.el" :package "shrink-path" :local-repo "shrink-path.el")) "s" ("2023-10-20 10:04:30" nil (:type git :flavor melpa :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "dash" ("2023-10-22 10:54:39" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "f" ("2023-10-11 21:04:16" ("emacs" "s" "dash") (:type git :flavor melpa :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "ef-themes" ("2023-10-11 21:04:16" ("emacs") (:type git :host github :repo "emacs-straight/ef-themes" :files ("*" (:exclude ".git")) :package "ef-themes" :local-repo "ef-themes")) "palimpsest" ("2023-10-11 21:04:16" nil (:type git :flavor melpa :host github :repo "danielsz/Palimpsest" :package "palimpsest" :local-repo "Palimpsest")) "langtool" ("2023-10-11 21:04:16" ("emacs") (:type git :flavor melpa :files ("langtool.el" "langtool-pkg.el") :host github :repo "mhayashi1120/Emacs-langtool" :package "langtool" :local-repo "Emacs-langtool")) "rainbow-delimiters" ("2023-10-11 21:04:16" nil (:type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters" :package "rainbow-delimiters" :local-repo "rainbow-delimiters")) "org" ("2023-10-20 10:04:30" ("emacs") (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")) :package "org")) "org-tracktable" ("2023-10-11 21:04:16" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "tty-tourist/org-tracktable" :package "org-tracktable" :local-repo "org-tracktable")) "org-sticky-header" ("2023-10-11 21:04:17" ("emacs" "org") (:type git :flavor melpa :host github :repo "alphapapa/org-sticky-header" :package "org-sticky-header" :local-repo "org-sticky-header")) "projectile" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "bbatsov/projectile" :package "projectile" :local-repo "projectile")) "vertico" ("2023-10-13 08:30:48" ("emacs" "compat") (:type git :flavor melpa :files (:defaults "extensions/vertico-*.el" "vertico-pkg.el") :host github :repo "minad/vertico" :package "vertico" :local-repo "vertico")) "orderless" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "oantolin/orderless" :package "orderless" :local-repo "orderless")) "marginalia" ("2023-10-11 21:04:17" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/marginalia" :package "marginalia" :local-repo "marginalia")) "consult" ("2023-10-11 21:04:17" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/consult" :package "consult" :local-repo "consult")) "night-owl-theme" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "aaronjensen/night-owl-emacs" :package "night-owl-theme" :local-repo "night-owl-emacs")) "embark" ("2023-10-11 21:04:17" ("emacs" "compat") (:type git :flavor melpa :files ("embark.el" "embark-org.el" "embark.texi" "embark-pkg.el") :host github :repo "oantolin/embark" :package "embark" :local-repo "embark")) "embark-consult" ("2023-10-11 21:04:17" ("emacs" "embark" "consult") (:flavor melpa :files ("embark-consult.el" "embark-consult-pkg.el") :package "embark-consult" :local-repo "embark" :type git :repo "oantolin/embark" :host github)) "consult-company" ("2023-10-11 21:04:17" ("emacs" "company" "consult") (:type git :flavor melpa :host github :repo "mohkale/consult-company" :package "consult-company" :local-repo "consult-company")) "crux" ("2023-10-11 21:04:17" ("seq") (:type git :flavor melpa :host github :repo "bbatsov/crux" :package "crux" :local-repo "crux")) "company-wordfreq" ("2023-10-11 21:04:17" ("emacs" "company") (:type git :flavor melpa :host github :repo "johannes-mueller/company-wordfreq.el" :package "company-wordfreq" :local-repo "company-wordfreq.el")) "eros" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "xiongtx/eros" :package "eros" :local-repo "eros")) "adoc-mode" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "bbatsov/adoc-mode" :package "adoc-mode" :local-repo "adoc-mode")) "which-key" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "justbur/emacs-which-key" :package "which-key" :local-repo "emacs-which-key")) "magit" ("2023-10-22 10:54:41" ("emacs" "compat" "dash" "git-commit" "magit-section" "seq" "transient" "with-editor") (:type git :flavor melpa :files ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit" :package "magit" :local-repo "magit")) "git-commit" ("2023-10-22 10:54:41" ("emacs" "compat" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2023-10-22 10:54:41" ("emacs" "compat") (:type git :flavor melpa :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2023-10-22 10:54:41" ("emacs" "compat") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "magit-section" ("2023-10-22 10:54:41" ("emacs" "compat" "dash") (:flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi" "magit-section-pkg.el") :package "magit-section" :local-repo "magit" :type git :repo "magit/magit" :host github)) "lispy" ("2023-10-11 21:04:17" ("emacs" "ace-window" "iedit" "swiper" "hydra" "zoutline") (:type git :flavor melpa :files (:defaults "lispy-clojure.clj" "lispy-clojure.cljs" "lispy-python.py" "lispy-pkg.el") :host github :repo "abo-abo/lispy" :package "lispy" :local-repo "lispy")) "ace-window" ("2023-10-11 21:04:17" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "avy" ("2023-10-11 21:04:17" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "iedit" ("2023-10-11 21:04:17" nil (:type git :flavor melpa :host github :repo "victorhge/iedit" :package "iedit" :local-repo "iedit")) "swiper" ("2023-10-11 21:04:17" ("emacs" "ivy") (:type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper" :package "swiper" :local-repo "swiper")) "ivy" ("2023-10-11 21:04:17" ("emacs") (:flavor melpa :files (:defaults "doc/ivy-help.org" (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "ivy-pkg.el") :package "ivy" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "hydra" ("2023-10-11 21:04:17" ("cl-lib" "lv") (:type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra" :package "hydra" :local-repo "hydra")) "lv" ("2023-10-11 21:04:17" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "zoutline" ("2023-10-11 21:04:17" nil (:type git :flavor melpa :host github :repo "abo-abo/zoutline" :package "zoutline" :local-repo "zoutline")) "git-gutter" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/git-gutter" :package "git-gutter" :local-repo "git-gutter")) "poet-theme" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "kunalb/poet" :package "poet-theme" :local-repo "poet")) "org-modern" ("2023-10-11 21:04:17" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/org-modern" :package "org-modern" :local-repo "org-modern")) "centered-cursor-mode" ("2023-10-11 21:04:17" nil (:type git :flavor melpa :host github :repo "andre-r/centered-cursor-mode.el" :package "centered-cursor-mode" :local-repo "centered-cursor-mode.el")) "olivetti" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "rnkn/olivetti" :package "olivetti" :local-repo "olivetti")) "beacon" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "Malabarba/beacon" :package "beacon" :local-repo "beacon")) "mixed-pitch" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host gitlab :repo "jabranham/mixed-pitch" :package "mixed-pitch" :local-repo "mixed-pitch")) "flycheck" ("2023-10-11 21:04:17" ("emacs" "dash" "pkg-info" "let-alist" "seq") (:type git :flavor melpa :host github :repo "flycheck/flycheck" :package "flycheck" :local-repo "flycheck")) "pkg-info" ("2023-10-11 21:04:17" ("epl") (:type git :flavor melpa :host github :repo "emacsorphanage/pkg-info" :package "pkg-info" :local-repo "pkg-info")) "epl" ("2023-10-11 21:04:17" ("cl-lib") (:type git :flavor melpa :host github :repo "cask/epl" :package "epl" :local-repo "epl")) "let-alist" ("2023-10-22 10:54:40" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "consult-flyspell" ("2023-10-11 21:04:17" ("emacs" "consult") (:type git :flavor melpa :host gitlab :repo "OlMon/consult-flyspell" :package "consult-flyspell" :local-repo "consult-flyspell")) "company-posframe" ("2023-10-11 21:04:17" ("emacs" "company" "posframe") (:type git :flavor melpa :host github :repo "tumashu/company-posframe" :package "company-posframe" :local-repo "company-posframe")) "posframe" ("2023-10-13 08:30:48" ("emacs") (:type git :flavor melpa :host github :repo "tumashu/posframe" :package "posframe" :local-repo "posframe")) "yasnippets" ("2023-10-10 16:50:21" nil (:type git :host github :repo "rejeep/yasnippets" :files (:defaults) :package "yasnippets" :local-repo "yasnippets")) "company-box" ("2023-10-11 21:04:17" ("emacs" "dash" "company" "frame-local") (:type git :flavor melpa :files (:defaults "images" "company-box-pkg.el") :host github :repo "sebastiencs/company-box" :package "company-box" :local-repo "company-box")) "frame-local" ("2023-10-11 21:04:17" ("emacs") (:type git :flavor melpa :host github :repo "sebastiencs/frame-local" :package "frame-local" :local-repo "frame-local")) "hyperbole" ("2023-10-12 18:21:16" ("emacs") (:type git :host github :repo "emacs-straight/hyperbole" :files ("*" (:exclude ".git")) :package "hyperbole" :local-repo "hyperbole")) "vertico-posframe" ("2023-10-13 08:30:48" ("emacs" "posframe" "vertico") (:type git :host github :repo "emacs-straight/vertico-posframe" :files ("*" (:exclude ".git")) :package "vertico-posframe" :local-repo "vertico-posframe")) "org-appear" ("2023-10-13 13:36:15" ("emacs" "org") (:type git :flavor melpa :host github :repo "awth13/org-appear" :package "org-appear" :local-repo "org-appear")) "eshell-git-prompt" ("2023-10-17 20:43:37" ("emacs" "cl-lib" "dash") (:type git :flavor melpa :host github :repo "xuchunyang/eshell-git-prompt" :package "eshell-git-prompt" :local-repo "eshell-git-prompt")) "eshell-syntax-highlighting" ("2023-10-17 20:44:26" ("emacs") (:type git :flavor melpa :host github :repo "akreisher/eshell-syntax-highlighting" :package "eshell-syntax-highlighting" :local-repo "eshell-syntax-highlighting")) "company-shell" ("2023-10-17 20:56:08" ("emacs" "company" "dash" "cl-lib") (:type git :flavor melpa :host github :repo "Alexander-Miller/company-shell" :package "company-shell" :local-repo "company-shell")) "pdf-tools" ("2023-10-17 20:56:53" ("emacs" "tablist" "let-alist") (:type git :flavor melpa :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools" :package "pdf-tools" :local-repo "pdf-tools")) "tablist" ("2023-10-17 20:56:52" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/tablist" :package "tablist" :local-repo "tablist")) "nov" ("2023-10-17 20:57:25" ("esxml" "emacs") (:type git :flavor melpa :repo "https://depp.brause.cc/nov.el.git" :package "nov" :local-repo "nov.el")) "esxml" ("2023-10-20 10:04:32" ("emacs" "kv" "cl-lib") (:type git :flavor melpa :files ("esxml.el" "esxml-query.el" "esxml-pkg.el") :host github :repo "tali713/esxml" :package "esxml" :local-repo "esxml")) "kv" ("2023-10-20 10:04:32" nil (:type git :flavor melpa :host github :repo "nicferrier/emacs-kv" :package "kv" :local-repo "emacs-kv")) "ox-pandoc" ("2023-10-17 21:13:47" ("org" "emacs" "dash" "ht") (:type git :flavor melpa :host github :repo "emacsorphanage/ox-pandoc" :package "ox-pandoc" :local-repo "ox-pandoc")) "ht" ("2023-10-17 21:13:47" ("dash") (:type git :flavor melpa :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "org-view-mode" ("2023-10-19 21:13:14" ("emacs") (:type git :flavor melpa :host github :repo "amno1/org-view-mode" :package "org-view-mode" :local-repo "org-view-mode")) "calibredb" ("2023-10-20 10:04:33" ("emacs" "org" "transient" "s" "dash" "request" "esxml") (:type git :flavor melpa :host github :repo "chenyanming/calibredb.el" :package "calibredb" :local-repo "calibredb.el")) "request" ("2023-10-20 10:04:32" ("emacs") (:type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request" :package "request" :local-repo "emacs-request")) "ox-hugo" ("2023-10-20 16:36:17" ("emacs" "tomelr") (:type git :flavor melpa :host github :repo "kaushalmodi/ox-hugo" :package "ox-hugo" :local-repo "ox-hugo")) "tomelr" ("2023-10-20 16:36:16" ("emacs" "map" "seq") (:type git :host github :repo "emacs-straight/tomelr" :files ("*" (:exclude ".git")) :package "tomelr" :local-repo "tomelr")) "map" ("2023-10-20 16:36:15" ("emacs") (:type git :host github :repo "emacs-straight/map" :files ("*" (:exclude ".git")) :package "map" :local-repo "map")) "pulsar" ("2023-10-20 17:09:49" ("emacs") (:type git :host github :repo "emacs-straight/pulsar" :files ("*" (:exclude ".git")) :package "pulsar" :local-repo "pulsar")) "multi-eshell" ("2023-10-20 17:27:39" nil (:type git :host github :repo "emacsmirror/multi-eshell" :package "multi-eshell" :local-repo "multi-eshell")) "forge" ("2023-10-22 10:54:53" ("emacs" "compat" "closql" "dash" "emacsql" "ghub" "let-alist" "magit" "markdown-mode" "seq" "transient" "yaml") (:type git :flavor melpa :host github :repo "magit/forge" :package "forge" :local-repo "forge")) "closql" ("2023-10-22 10:54:39" ("emacs" "compat" "emacsql") (:type git :flavor melpa :host github :repo "magit/closql" :package "closql" :local-repo "closql")) "emacsql" ("2023-10-22 10:54:38" ("emacs") (:type git :flavor melpa :files (:defaults "sqlite" "emacsql-pkg.el") :host github :repo "magit/emacsql" :package "emacsql" :local-repo "emacsql")) "ghub" ("2023-10-22 10:54:41" ("emacs" "compat" "let-alist" "treepy") (:type git :flavor melpa :host github :repo "magit/ghub" :package "ghub" :local-repo "ghub")) "treepy" ("2023-10-22 10:54:41" ("emacs") (:type git :flavor melpa :host github :repo "volrath/treepy.el" :package "treepy" :local-repo "treepy.el")) "markdown-mode" ("2023-10-22 10:54:43" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "yaml" ("2023-10-22 10:54:50" ("emacs") (:type git :flavor melpa :host github :repo "zkry/yaml.el" :package "yaml" :local-repo "yaml.el"))))

#s(hash-table size 97 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight-x straight-autoloads straight straight-ert-print-hack) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos and build directories.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

Optional arg FILTER must be a unary function.
It takes a package name as its sole argument.
If it returns nil the candidate is excluded.

(fn &optional SOURCES ACTION FILTER)" t) (autoload 'straight-visit-package-website "straight" "Visit the package RECIPE's website.

(fn RECIPE)" t) (autoload 'straight-visit-package "straight" "Open PACKAGE's local repository directory.
When BUILD is non-nil visit PACKAGE's build directory.

(fn PACKAGE &optional BUILD)" t) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil when package is initially installed, nil otherwise.

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded.") (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted.") (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package \\='(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function 0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t) (autoload 'straight-dependents "straight" "Return a list of PACKAGE's dependents.

(fn &optional PACKAGE)" t) (register-definition-prefixes "straight" '("straight-")) (register-definition-prefixes "straight-ert-print-hack" '("+without-print-limits")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "diminish" ((diminish diminish-autoloads) (autoload 'diminish "diminish" "Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish \\='jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have `abbrev-mode' enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space.

(fn MODE &optional TO-WHAT)" t) (autoload 'diminish-undo "diminish" "Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked \\[diminish]).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo \\='diminished-modes).

(fn MODE)" t) (autoload 'diminished-modes "diminish" "Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor." t) (register-definition-prefixes "diminish" '("diminish")) (provide 'diminish-autoloads)) "company" ((company-dabbrev-code company-tempo company-gtags company-elisp company-autoloads company-etags company-ispell company-capf company-yasnippet company-template company-nxml company-css company-clang company company-files company-abbrev company-keywords company-cmake company-semantic company-tng company-bbdb company-oddmuse company-dabbrev) (autoload 'company-mode "company" "\"complete anything\"; is an in-buffer completion framework.

Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

This is a minor mode.  If called interactively, toggle the
`Company mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `company-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-company-mode 'globalized-minor-mode t) (defvar global-company-mode nil "Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.") (custom-autoload 'global-company-mode "company" nil) (autoload 'global-company-mode "company" "Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Company mode is enabled in all buffers where `company-mode-on' would
do it.

See `company-mode' for more information on Company mode.

(fn &optional ARG)" t) (autoload 'company-manual-begin "company" nil t) (autoload 'company-complete "company" "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t) (register-definition-prefixes "company" '("company-")) (autoload 'company-abbrev "company-abbrev" "`company-mode' completion backend for abbrev.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert")) (autoload 'company-bbdb "company-bbdb" "`company-mode' completion backend for BBDB.

(fn COMMAND &optional ARG &rest IGNORE)" t) (register-definition-prefixes "company-bbdb" '("company-bbdb-")) (register-definition-prefixes "company-capf" '("company-")) (register-definition-prefixes "company-clang" '("company-clang")) (register-definition-prefixes "company-cmake" '("company-cmake")) (autoload 'company-css "company-css" "`company-mode' completion backend for `css-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-css" '("company-css-")) (autoload 'company-dabbrev "company-dabbrev" "dabbrev-like `company-mode' completion backend.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-")) (autoload 'company-dabbrev-code "company-dabbrev-code" "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-")) (autoload 'company-elisp "company-elisp" "`company-mode' completion backend for Emacs Lisp.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-elisp" '("company-elisp-")) (autoload 'company-etags "company-etags" "`company-mode' completion backend for etags.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-etags" '("company-etags-")) (autoload 'company-files "company-files" "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-files" '("company-file")) (autoload 'company-gtags "company-gtags" "`company-mode' completion backend for GNU Global.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-gtags" '("company-gtags-")) (autoload 'company-ispell "company-ispell" "`company-mode' completion backend using Ispell.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-ispell" '("company-ispell-")) (autoload 'company-keywords "company-keywords" "`company-mode' backend for programming language keywords.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-keywords" '("company-keywords-")) (autoload 'company-nxml "company-nxml" "`company-mode' completion backend for `nxml-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-nxml" '("company-nxml-")) (autoload 'company-oddmuse "company-oddmuse" "`company-mode' completion backend for `oddmuse-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-")) (autoload 'company-semantic "company-semantic" "`company-mode' completion backend using CEDET Semantic.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-semantic" '("company-semantic-")) (register-definition-prefixes "company-template" '("company-template-")) (autoload 'company-tempo "company-tempo" "`company-mode' completion backend for tempo.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-tempo" '("company-tempo-")) (autoload 'company-tng-frontend "company-tng" "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

(fn COMMAND)") (define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.9.14" "Applies the default configuration to enable company-tng.") (defvar company-tng-mode nil "Non-nil if Company-Tng mode is enabled.
See the `company-tng-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tng-mode'.") (custom-autoload 'company-tng-mode "company-tng" nil) (autoload 'company-tng-mode "company-tng" "This minor mode enables `company-tng-frontend'.

This is a global minor mode.  If called interactively, toggle the
`Company-Tng mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='company-tng-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "company-tng" '("company-tng-")) (autoload 'company-yasnippet "company-yasnippet" "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)

(fn COMMAND &optional ARG &rest IGNORE)" t) (register-definition-prefixes "company-yasnippet" '("company-yasnippet-")) (provide 'company-autoloads)) "gcmh" ((gcmh gcmh-autoloads) (defvar gcmh-mode nil "Non-nil if GCMH mode is enabled.
See the `gcmh-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `gcmh-mode'.") (custom-autoload 'gcmh-mode "gcmh" nil) (autoload 'gcmh-mode "gcmh" "Minor mode to tweak Garbage Collection strategy.

This is a global minor mode.  If called interactively, toggle the
`GCMH mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='gcmh-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "gcmh" '("gcmh-")) (provide 'gcmh-autoloads)) "objed" ((objed-objects objed-autoloads objed) (autoload 'objed-activate "objed" "Activate objed.

When called non interactively activate with object OBJ which
defaults to char object. Otherwise uses associated
`objed-cmd-alist' for `last-command' as initial object. Falls
back to `objed-initial-object' if no match found.

(fn &optional OBJ)" t) (autoload 'objed-activate-object "objed" "Query for object and activate with it." t) (autoload 'objed-beg-of-object-at-point "objed" "Activate and move to beginning of object at point.

On repeat or at boundary move to previous." t) (autoload 'objed-end-of-object-at-point "objed" "Activate and move to end of object at point.

On repeat or at boundary move to next." t) (autoload 'objed-until-beg-of-object-at-point "objed" "Move to beginning of object at point and active text moved over." t) (autoload 'objed-until-end-of-object-at-point "objed" "Move to end of object at point and active text moved over." t) (autoload 'objed-first-identifier "objed" "Move to first instance of identifier at point." t) (autoload 'objed-last-identifier "objed" "Move to last instance of identifier at point." t) (autoload 'objed-next-identifier "objed" "Activate object with identifier at point." t) (autoload 'objed-prev-identifier "objed" "Activate object with identifier at point." t) (defvar objed-mode nil "Non-nil if Objed mode is enabled.
See the `objed-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `objed-mode'.") (custom-autoload 'objed-mode "objed" nil) (autoload 'objed-mode "objed" "Enable objeds modal editing features after certain commands.

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

(fn &optional ARG)" t) (register-definition-prefixes "objed" '("objed-")) (register-definition-prefixes "objed-objects" '("objed-")) (provide 'objed-autoloads)) "solaire-mode" ((solaire-mode-autoloads solaire-mode) (defface solaire-default-face '((t :inherit default)) "Alternative version of the `default' face." :group 'solaire-mode) (autoload 'solaire-mode "solaire-mode" "Make current buffer a different color so others can be grossly incandescent.

Remaps faces in `solaire-mode-remap-alist', then runs `solaire-mode-hook', where
additional mode-specific fixes may live. Lastly, adjusts the fringes for the
current frame.

This is a minor mode.  If called interactively, toggle the
`Solaire mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `solaire-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'solaire-global-mode 'globalized-minor-mode t) (defvar solaire-global-mode nil "Non-nil if Solaire-Global mode is enabled.
See the `solaire-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `solaire-global-mode'.") (custom-autoload 'solaire-global-mode "solaire-mode" nil) (autoload 'solaire-global-mode "solaire-mode" "Toggle Solaire mode in all buffers.
With prefix ARG, enable Solaire-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Solaire mode is enabled in all buffers where `turn-on-solaire-mode'
would do it.

See `solaire-mode' for more information on Solaire mode.

(fn &optional ARG)" t) (autoload 'turn-on-solaire-mode "solaire-mode" "Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'.

(fn &rest _)" t) (autoload 'turn-off-solaire-mode "solaire-mode" "Disable `solaire-mode' in the current buffer.

(fn &rest _)" t) (autoload 'solaire-mode-reset "solaire-mode" "Reset `solaire-mode' in all buffers where it is enabled.

Use this in case solaire-mode has caused some sort of problem, e.g. after
changing themes.  are more prelevant in Emacs 25 and 26, but far less so in 27+;
particularly where the fringe is concerned.

(fn &rest _)" t) (autoload 'solaire-mode-reset-buffer "solaire-mode" "Reset `solaire-mode' incurrent buffer.

See `solaire-mode-reset' for details.") (defun solaire-mode--prepare-for-theme-a (theme &rest _) "Prepare solaire-mode for THEME.
Meant to be used as a `load-theme' advice." (when (and (get theme 'theme-feature) (memq theme custom-enabled-themes)) (setq solaire-mode--supported-p (ignore-errors (let ((default1 (face-background 'default nil t)) (default2 (face-background 'solaire-default-face nil t))) (and default1 default2 (not (equal default1 default2))))) solaire-mode--swapped-p nil solaire-mode--theme theme) (when (bound-and-true-p solaire-global-mode) (if solaire-mode--supported-p (solaire-mode-swap-faces-maybe) (solaire-global-mode -1))))) (advice-add #'load-theme :after #'solaire-mode--prepare-for-theme-a) (register-definition-prefixes "solaire-mode" '("solaire-mode-")) (provide 'solaire-mode-autoloads)) "doom-themes" ((doom-ayu-dark-theme doom-rouge-theme doom-xcode-theme doom-sourcerer-theme doom-manegarm-theme doom-ir-black-theme doom-bluloco-light-theme doom-nord-theme doom-oksolar-dark-theme doom-palenight-theme doom-laserwave-theme doom-flatwhite-theme doom-ayu-light-theme doom-feather-light-theme doom-one-light-theme doom-acario-dark-theme doom-themes-autoloads doom-plain-theme doom-bluloco-dark-theme doom-old-hope-theme doom-lantern-theme doom-1337-theme doom-challenger-deep-theme doom-solarized-dark-high-contrast-theme doom-monokai-ristretto-theme doom-opera-light-theme doom-nord-light-theme doom-themes-ext-neotree doom-one-theme doom-miramare-theme doom-zenburn-theme doom-horizon-theme doom-oksolar-light-theme doom-ephemeral-theme doom-themes-ext-treemacs doom-nord-aurora-theme doom-nova-theme doom-themes-ext-org doom-monokai-machine-theme doom-feather-dark-theme doom-homage-black-theme doom-monokai-pro-theme doom-outrun-electric-theme doom-tomorrow-night-theme doom-meltbus-theme doom-city-lights-theme doom-solarized-dark-theme doom-shades-of-purple-theme doom-themes-base doom-dark+-theme doom-oceanic-next-theme doom-snazzy-theme doom-themes-ext-visual-bell doom-acario-light-theme doom-tomorrow-day-theme doom-vibrant-theme doom-molokai-theme doom-wilmersdorf-theme doom-tokyo-night-theme doom-material-dark-theme doom-badger-theme doom-gruvbox-theme doom-henna-theme doom-pine-theme doom-dracula-theme doom-ayu-mirage-theme doom-opera-theme doom-monokai-spectrum-theme doom-solarized-light-theme doom-moonlight-theme doom-Iosvkem-theme doom-earl-grey-theme doom-plain-dark-theme doom-peacock-theme doom-material-theme doom-fairy-floss-theme doom-homage-white-theme doom-spacegrey-theme doom-monokai-octagon-theme doom-themes doom-monokai-classic-theme doom-gruvbox-light-theme) (register-definition-prefixes "doom-1337-theme" '("doom-1337")) (register-definition-prefixes "doom-Iosvkem-theme" '("doom-Iosvkem")) (register-definition-prefixes "doom-acario-dark-theme" '("doom-acario-dark")) (register-definition-prefixes "doom-acario-light-theme" '("doom-acario-light")) (register-definition-prefixes "doom-ayu-dark-theme" '("doom-ayu-dark")) (register-definition-prefixes "doom-ayu-light-theme" '("doom-ayu-light")) (register-definition-prefixes "doom-ayu-mirage-theme" '("doom-ayu-mirage")) (register-definition-prefixes "doom-badger-theme" '("doom-badger")) (register-definition-prefixes "doom-bluloco-dark-theme" '("doom-bluloco-dark")) (register-definition-prefixes "doom-bluloco-light-theme" '("doom-bluloco-light")) (register-definition-prefixes "doom-challenger-deep-theme" '("doom-challenger-deep")) (register-definition-prefixes "doom-city-lights-theme" '("doom-city-lights")) (register-definition-prefixes "doom-dark+-theme" '("doom-dark+")) (register-definition-prefixes "doom-dracula-theme" '("doom-dracula")) (register-definition-prefixes "doom-earl-grey-theme" '("doom-earl-grey")) (register-definition-prefixes "doom-ephemeral-theme" '(":group" "doom-ephemeral")) (register-definition-prefixes "doom-fairy-floss-theme" '("doom-fairy-floss")) (register-definition-prefixes "doom-feather-dark-theme" '("doom-feather-")) (register-definition-prefixes "doom-feather-light-theme" '("doom-feather-light")) (register-definition-prefixes "doom-flatwhite-theme" '("doom-f")) (register-definition-prefixes "doom-gruvbox-light-theme" '("doom-gruvbox-light")) (register-definition-prefixes "doom-gruvbox-theme" '("doom-gruvbox")) (register-definition-prefixes "doom-henna-theme" '("doom-henna")) (register-definition-prefixes "doom-homage-black-theme" '("doom-homage-black")) (register-definition-prefixes "doom-homage-white-theme" '("doom-homage-white")) (register-definition-prefixes "doom-horizon-theme" '("doom-horizon")) (register-definition-prefixes "doom-ir-black-theme" '("doom-ir-black")) (register-definition-prefixes "doom-lantern-theme" '("doom-lantern")) (register-definition-prefixes "doom-laserwave-theme" '("doom-laserwave")) (register-definition-prefixes "doom-manegarm-theme" '("doom-manegarm")) (register-definition-prefixes "doom-material-dark-theme" '("doom-material-")) (register-definition-prefixes "doom-material-theme" '("doom-material")) (register-definition-prefixes "doom-meltbus-theme" '("doom-meltbus")) (register-definition-prefixes "doom-miramare-theme" '("doom-miramare")) (register-definition-prefixes "doom-molokai-theme" '("doom-molokai")) (register-definition-prefixes "doom-monokai-classic-theme" '("doom-monokai-classic")) (register-definition-prefixes "doom-monokai-machine-theme" '("doom-monokai-machine")) (register-definition-prefixes "doom-monokai-octagon-theme" '("doom-monokai-octagon")) (register-definition-prefixes "doom-monokai-pro-theme" '("doom-monokai-pro")) (register-definition-prefixes "doom-monokai-ristretto-theme" '("doom-monokai-ristretto")) (register-definition-prefixes "doom-monokai-spectrum-theme" '("doom-monokai-spectrum")) (register-definition-prefixes "doom-moonlight-theme" '("doom-moonlight")) (register-definition-prefixes "doom-nord-aurora-theme" '(":group" "doom-nord-aurora")) (register-definition-prefixes "doom-nord-light-theme" '(":group" "doom-nord-light")) (register-definition-prefixes "doom-nord-theme" '(":group" "doom-nord")) (register-definition-prefixes "doom-nova-theme" '("doom-nova")) (register-definition-prefixes "doom-oceanic-next-theme" '("doom-oceanic-next")) (register-definition-prefixes "doom-oksolar-dark-theme" '("doom-oksolar-dark")) (register-definition-prefixes "doom-oksolar-light-theme" '("doom-oksolar-light")) (register-definition-prefixes "doom-old-hope-theme" '("doom-old-hope")) (register-definition-prefixes "doom-one-light-theme" '("doom-one-light")) (register-definition-prefixes "doom-one-theme" '("doom-one")) (register-definition-prefixes "doom-opera-light-theme" '(":group" "doom-opera-light")) (register-definition-prefixes "doom-opera-theme" '(":group" "doom-opera")) (register-definition-prefixes "doom-outrun-electric-theme" '("doom-outrun-electric")) (register-definition-prefixes "doom-palenight-theme" '("doom-palenight")) (register-definition-prefixes "doom-peacock-theme" '("doom-peacock")) (register-definition-prefixes "doom-pine-theme" '("doom-pine")) (register-definition-prefixes "doom-plain-dark-theme" '("doom-plain-")) (register-definition-prefixes "doom-plain-theme" '("doom-plain")) (register-definition-prefixes "doom-rouge-theme" '("doom-rouge")) (register-definition-prefixes "doom-shades-of-purple-theme" '("doom-shades-of-purple")) (register-definition-prefixes "doom-snazzy-theme" '("doom-snazzy")) (register-definition-prefixes "doom-solarized-dark-high-contrast-theme" '("doom-solarized-dark-high-contrast")) (register-definition-prefixes "doom-solarized-dark-theme" '("doom-solarized-dark")) (register-definition-prefixes "doom-solarized-light-theme" '("doom-solarized-light")) (register-definition-prefixes "doom-sourcerer-theme" '("doom-sourcerer")) (register-definition-prefixes "doom-spacegrey-theme" '("doom-spacegrey")) (autoload 'doom-name-to-rgb "doom-themes" "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame).

(fn COLOR)") (autoload 'doom-blend "doom-themes" "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)

(fn COLOR1 COLOR2 ALPHA)") (autoload 'doom-darken "doom-themes" "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1).

(fn COLOR ALPHA)") (autoload 'doom-lighten "doom-themes" "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1).

(fn COLOR ALPHA)") (autoload 'doom-color "doom-themes" "Retrieve a specific color named NAME (a symbol) from the current theme.

(fn NAME &optional TYPE)") (autoload 'doom-ref "doom-themes" "TODO

(fn FACE PROP &optional CLASS)") (autoload 'doom-themes-set-faces "doom-themes" "Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of Doom
theme face specs. These is a simplified spec. For example:

  (doom-themes-set-faces \\='user
    \\='(default :background red :foreground blue)
    \\='(doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    \\='(doom-modeline-buffer-file :inherit \\='mode-line-buffer-id :weight \\='bold)
    \\='(doom-modeline-buffer-path :inherit \\='mode-line-emphasis :weight \\='bold)
    \\='(doom-modeline-buffer-project-root :foreground green :weight \\='bold))

(fn THEME &rest FACES)") (function-put 'doom-themes-set-faces 'lisp-indent-function 'defun) (when (and (boundp 'custom-theme-load-path) load-file-name) (let* ((base (file-name-directory load-file-name)) (dir (expand-file-name "themes/" base))) (add-to-list 'custom-theme-load-path (or (and (file-directory-p dir) dir) base)))) (register-definition-prefixes "doom-themes" '("def-doom-theme" "doom-")) (register-definition-prefixes "doom-themes-base" '("doom-themes-base-")) (autoload 'doom-themes-neotree-config "doom-themes-ext-neotree" "Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.") (register-definition-prefixes "doom-themes-ext-neotree" '("doom-")) (autoload 'doom-themes-org-config "doom-themes-ext-org" "Load `doom-themes-ext-org'.") (register-definition-prefixes "doom-themes-ext-org" '("doom-themes-")) (autoload 'doom-themes-treemacs-config "doom-themes-ext-treemacs" "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.") (register-definition-prefixes "doom-themes-ext-treemacs" '("doom-themes-")) (autoload 'doom-themes-visual-bell-fn "doom-themes-ext-visual-bell" "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it.") (autoload 'doom-themes-visual-bell-config "doom-themes-ext-visual-bell" "Enable flashing the mode-line on error.") (register-definition-prefixes "doom-tokyo-night-theme" '("doom-tokyo-night")) (register-definition-prefixes "doom-tomorrow-day-theme" '("doom-tomorrow-day")) (register-definition-prefixes "doom-tomorrow-night-theme" '("doom-tomorrow-night")) (register-definition-prefixes "doom-vibrant-theme" '("doom-vibrant")) (register-definition-prefixes "doom-wilmersdorf-theme" '("doom-wilmersdorf")) (register-definition-prefixes "doom-xcode-theme" '("doom-xcode")) (register-definition-prefixes "doom-zenburn-theme" '("doom-zenburn")) (provide 'doom-themes-autoloads)) "compat" ((compat-27 compat-26 compat-29 compat-macs compat-tests compat-28 compat compat-autoloads compat-25) (register-definition-prefixes "compat" '("compat-")) (register-definition-prefixes "compat-macs" '("compat-")) (register-definition-prefixes "compat-tests" '("compat-tests--" "should-equal")) (provide 'compat-autoloads)) "nerd-icons" ((nerd-icons-autoloads nerd-icons-data nerd-icons nerd-icons-faces) (autoload 'nerd-icons-install-fonts "nerd-icons" "Helper function to download and install the latests fonts based on OS.
The provided Nerd Font is Symbols Nerd Font Mono.
When PFX is non-nil, ignore the prompt and just install

(fn &optional PFX)" t) (autoload 'nerd-icons-insert "nerd-icons" "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When GLYPH-SET is non-nil, limit the candidates to the icon set matching it.

(fn &optional ARG GLYPH-SET)" t) (autoload 'nerd-icons-icon-for-dir "nerd-icons" "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn DIR &rest ARG-OVERRIDES)") (autoload 'nerd-icons-icon-for-file "nerd-icons" "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn FILE &rest ARG-OVERRIDES)") (autoload 'nerd-icons-icon-for-extension "nerd-icons" "Get the formatted icon for EXT.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn EXT &rest ARG-OVERRIDES)") (autoload 'nerd-icons-icon-for-mode "nerd-icons" "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn MODE &rest ARG-OVERRIDES)") (autoload 'nerd-icons-icon-for-url "nerd-icons" "Get the formatted icon for URL.
If an icon for URL isn't found in `nerd-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn URL &rest ARG-OVERRIDES)") (autoload 'nerd-icons-icon-for-buffer "nerd-icons" "Get the formatted icon for the current buffer.

This function prioritises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon.") (register-definition-prefixes "nerd-icons" '("nerd-icons-")) (provide 'nerd-icons-autoloads)) "s" ((s s-autoloads) (register-definition-prefixes "s" '("s-")) (provide 's-autoloads)) "dash" ((dash dash-autoloads) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

This is a minor mode.  If called interactively, toggle the
`Dash-Fontify mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dash-fontify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-")) (provide 'dash-autoloads)) "f" ((f f-autoloads f-shortdoc) (register-definition-prefixes "f" '("f-")) (provide 'f-autoloads)) "shrink-path" ((shrink-path shrink-path-autoloads) (register-definition-prefixes "shrink-path" '("shrink-path-")) (provide 'shrink-path-autoloads)) "doom-modeline" ((doom-modeline doom-modeline-autoloads doom-modeline-core doom-modeline-segments doom-modeline-env) (autoload 'doom-modeline-set-main-modeline "doom-modeline" "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers.

(fn &optional DEFAULT)") (defvar doom-modeline-mode nil "Non-nil if Doom-Modeline mode is enabled.
See the `doom-modeline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `doom-modeline-mode'.") (custom-autoload 'doom-modeline-mode "doom-modeline" nil) (autoload 'doom-modeline-mode "doom-modeline" "Toggle `doom-modeline' on or off.

This is a global minor mode.  If called interactively, toggle the
`Doom-Modeline mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='doom-modeline-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "doom-modeline" '("doom-modeline-")) (register-definition-prefixes "doom-modeline-core" '("doom-modeline")) (autoload 'doom-modeline-env-setup-python "doom-modeline-env") (autoload 'doom-modeline-env-setup-ruby "doom-modeline-env") (autoload 'doom-modeline-env-setup-perl "doom-modeline-env") (autoload 'doom-modeline-env-setup-go "doom-modeline-env") (autoload 'doom-modeline-env-setup-elixir "doom-modeline-env") (autoload 'doom-modeline-env-setup-rust "doom-modeline-env") (register-definition-prefixes "doom-modeline-env" '("doom-modeline-")) (register-definition-prefixes "doom-modeline-segments" '("doom-modeline-")) (provide 'doom-modeline-autoloads)) "ef-themes" ((ef-cyprus-theme ef-dark-theme ef-day-theme ef-themes ef-summer-theme ef-maris-light-theme ef-elea-dark-theme ef-frost-theme ef-light-theme theme-loaddefs ef-spring-theme ef-deuteranopia-dark-theme ef-kassio-theme ef-symbiosis-theme ef-themes-autoloads ef-tritanopia-light-theme ef-duo-light-theme ef-trio-light-theme ef-maris-dark-theme ef-bio-theme ef-duo-dark-theme ef-elea-light-theme ef-deuteranopia-light-theme ef-night-theme ef-cherie-theme \.dir-locals ef-autumn-theme ef-tritanopia-dark-theme ef-winter-theme ef-trio-dark-theme) (autoload 'ef-themes-select "ef-themes" "Load an Ef THEME using minibuffer completion.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `ef-themes-post-load-hook' after loading the theme.

When called from Lisp, THEME is the symbol of a theme.  VARIANT
is ignored in this scenario.

(fn THEME &optional VARIANT)" t) (autoload 'ef-themes-select-light "ef-themes" "Load a light Ef THEME.
Run `ef-themes-post-load-hook' after loading the theme.

Also see `ef-themes-select-dark'.

This command is the same as `ef-themes-select' except it only
prompts for light themes when called interactively.  Calling it
from Lisp behaves the same as `ef-themes-select' for the THEME
argument, meaning that it loads the Ef THEME regardless of
whether it is light or dark.

(fn THEME)" t) (autoload 'ef-themes-select-dark "ef-themes" "Load a dark Ef THEME.
Run `ef-themes-post-load-hook' after loading the theme.

Also see `ef-themes-select-light'.

This command is the same as `ef-themes-select' except it only
prompts for dark themes when called interactively.  Calling it
from Lisp behaves the same as `ef-themes-select' for the THEME
argument, meaning that it loads the Ef THEME regardless of
whether it is light or dark.

(fn THEME)" t) (autoload 'ef-themes-toggle "ef-themes" "Toggle between the two `ef-themes-to-toggle'.
If `ef-themes-to-toggle' does not specify two Ef themes, inform
the user about it while prompting with completion for a theme
among our collection (this is practically the same as the
`ef-themes-select' command).

Run `ef-themes-post-load-hook' after loading the theme." t) (autoload 'ef-themes-load-random "ef-themes" "Load an Ef theme at random, excluding the current one.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `ef-themes-post-load-hook' after loading the theme.

When called from Lisp, VARIANT is either the `dark' or `light'
symbol.

(fn &optional VARIANT)" t) (autoload 'ef-themes-theme "ef-themes" "Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `ef-themes-faces' and
`ef-themes-custom-variables' respectively.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries.

(fn NAME PALETTE &optional OVERRIDES)" nil t) (function-put 'ef-themes-theme 'lisp-indent-function 0) (when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (file-equal-p dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir)))) (register-definition-prefixes "ef-themes" '("ef-themes-")) (provide 'ef-themes-autoloads)) "palimpsest" ((palimpsest-autoloads palimpsest) (autoload 'palimpsest-mode "palimpsest" "Toggle palimpsest mode.

Interactively with no argument, this command toggles the mode. You can customize
this minor mode, see option `palimpsest-mode'.

This is a minor mode.  If called interactively, toggle the
`Palimpsest mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `palimpsest-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "palimpsest" '("palimpsest-")) (provide 'palimpsest-autoloads)) "langtool" ((langtool-autoloads langtool) (defalias 'langtool-check #'langtool-check-buffer) (autoload 'langtool-check-buffer "langtool" "Check context current buffer and light up errors.
Optional \\[universal-argument] read LANG name.

You can change the `langtool-default-language' to apply all session.
Restrict to selection when region is activated.

(fn &optional LANG)" t) (autoload 'langtool-switch-default-language "langtool" "Switch `langtool-default-language' to LANG.

(fn LANG)" t) (register-definition-prefixes "langtool" '("langtool-")) (provide 'langtool-autoloads)) "rainbow-delimiters" ((rainbow-delimiters rainbow-delimiters-autoloads) (autoload 'rainbow-delimiters-mode "rainbow-delimiters" "Highlight nested parentheses, brackets, and braces according to their depth.

This is a minor mode.  If called interactively, toggle the
`Rainbow-Delimiters mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rainbow-delimiters-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "Enable `rainbow-delimiters-mode'.") (autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "Disable `rainbow-delimiters-mode'.") (register-definition-prefixes "rainbow-delimiters" '("rainbow-delimiters-")) (provide 'rainbow-delimiters-autoloads)) "org" ((ob-comint org-ctags ox-texinfo ol-doi org-refile org-version org-num ol-mhe ob-shell org-attach ob-C org-entities ob-dot ob-sql ol-eww org-datetree org-macro ob-eval ob-groovy ox-icalendar org-mobile ob-processing oc-csl ob-octave org-table ox-html ol ob-plantuml ol-docview org-fold-core ob-exp ob-ditaa ob-scheme oc-bibtex ol-gnus org-mouse org-inlinetask ob-fortran ob-haskell org-id oc ob-css org-compat org-element ob ob-calc ob-python org-agenda org-persist ob-ocaml org-tempo ob-ref ob-tangle org-fold ob-lilypond ob-clojure org-lint ol-eshell ox-latex org-pcomplete org-duration ox-beamer ol-info ob-table ol-rmail ox-odt org-protocol ob-latex ob-julia org-archive ol-w3m ob-sed ob-perl ob-eshell ol-man org-plot org-clock ob-awk org-list ob-sqlite org-macs ox-man ol-bbdb org-loaddefs ol-irc org org-capture org-colview org-goto ob-R ob-ruby ob-sass ob-matlab ox oc-natbib ob-core ob-makefile org-timer ob-org ob-forth oc-biblatex ob-lob ob-gnuplot org-faces org-habit ox-koma-letter ob-js ob-screen org-element-ast org-footnote oc-basic ol-bibtex ob-java ob-lua ox-md org-cycle ox-ascii org-keys ob-lisp org-attach-git ob-emacs-lisp ob-maxima org-crypt org-feed org-src ox-org org-indent ox-publish)) "org-tracktable" ((org-tracktable org-tracktable-autoloads) (autoload 'org-tracktable-insert-table "org-tracktable" "Insert the a table with the name defined by `org-tracktable-table-name'." t) (autoload 'org-tracktable-status "org-tracktable" "Report the number of words between positions BEG and END.
If a table is inserted with `org-tracktable-table-insert', shows words written today.
If `org-tracktable-daily-goal' is set to more than 0, show % of daily goal.

(fn BEG END)" t) (autoload 'org-tracktable-status-today "org-tracktable" "Reports number of words written today" t) (autoload 'org-tracktable-write "org-tracktable" "Write progress to the tracktable.
If the last entry is from today, this entry will be updated.
Otherwise a new entry will be made.  It is only necessary to call this function
when you're done writing for the day." t) (register-definition-prefixes "org-tracktable" '("org-tracktable-")) (provide 'org-tracktable-autoloads)) "org-sticky-header" ((org-sticky-header org-sticky-header-autoloads) (autoload 'org-sticky-header-mode "org-sticky-header" "Minor mode to show the current Org heading in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the minor mode is enabled.

(fn &optional ARG)" t) (register-definition-prefixes "org-sticky-header" '("org-sticky-header-")) (provide 'org-sticky-header-autoloads)) "projectile" ((projectile-autoloads projectile) (autoload 'projectile-version "projectile" "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t) (autoload 'projectile-invalidate-cache "projectile" "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

(fn PROMPT)" t) (autoload 'projectile-purge-file-from-cache "projectile" "Purge FILE from the cache of the current project.

(fn FILE)" t) (autoload 'projectile-purge-dir-from-cache "projectile" "Purge DIR from the cache of the current project.

(fn DIR)" t) (autoload 'projectile-cache-current-file "projectile" "Add the currently visited file to the cache." t) (autoload 'projectile-discover-projects-in-directory "projectile" "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there.

(fn DIRECTORY &optional DEPTH)" t) (autoload 'projectile-discover-projects-in-search-path "projectile" "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled." t) (autoload 'projectile-switch-to-buffer "projectile" "Switch to a project buffer." t) (autoload 'projectile-switch-to-buffer-other-window "projectile" "Switch to a project buffer and show it in another window." t) (autoload 'projectile-switch-to-buffer-other-frame "projectile" "Switch to a project buffer and show it in another frame." t) (autoload 'projectile-display-buffer "projectile" "Display a project buffer in another window without selecting it." t) (autoload 'projectile-project-buffers-other-buffer "projectile" "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned." t) (autoload 'projectile-multi-occur "projectile" "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

(fn &optional NLINES)" t) (autoload 'projectile-find-other-file "projectile" "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t) (autoload 'projectile-find-other-file-other-window "projectile" "Switch between files with different extensions in other window.
Switch between files with the same name but different extensions in other
window.  With FLEX-MATCHING, match any file that contains the base name of
current file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t) (autoload 'projectile-find-other-file-other-frame "projectile" "Switch between files with different extensions in other frame.
Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current
file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t) (autoload 'projectile-find-file-dwim "projectile" "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim' is executed on a filepath like
\"projectile/\", it lists the content of that directory.  If it is executed
on a partial filename like \"projectile/a\", a list of files with character
\"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-dwim-other-window "projectile" "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-window' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-dwim-other-frame "projectile" "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-frame' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file "projectile" "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-other-window "projectile" "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-other-frame "projectile" "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-toggle-project-read-only "projectile" "Toggle project read only." t) (autoload 'projectile-add-dir-local-variable "projectile" "Run `add-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to `add-dir-local-variable'.

(fn MODE VARIABLE VALUE)") (autoload 'projectile-delete-dir-local-variable "projectile" "Run `delete-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to
`delete-dir-local-variable'.

(fn MODE VARIABLE)") (autoload 'projectile-find-dir "projectile" "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-dir-other-window "projectile" "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-dir-other-frame "projectile" "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-test-file "projectile" "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-related-file-other-window "projectile" "Open related file in other window." t) (autoload 'projectile-find-related-file-other-frame "projectile" "Open related file in other frame." t) (autoload 'projectile-find-related-file "projectile" "Open related file." t) (autoload 'projectile-related-files-fn-groups "projectile" "Generate a related-files-fn which relates as KIND for files in each of GROUPS.

(fn KIND GROUPS)") (autoload 'projectile-related-files-fn-extensions "projectile" "Generate a related-files-fn which relates as KIND for files having EXTENSIONS.

(fn KIND EXTENSIONS)") (autoload 'projectile-related-files-fn-test-with-prefix "projectile" "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-PREFIX.

(fn EXTENSION TEST-PREFIX)") (autoload 'projectile-related-files-fn-test-with-suffix "projectile" "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-SUFFIX.

(fn EXTENSION TEST-SUFFIX)") (autoload 'projectile-project-info "projectile" "Display info for current project." t) (autoload 'projectile-find-implementation-or-test-other-window "projectile" "Open matching implementation or test file in other window.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined." t) (autoload 'projectile-find-implementation-or-test-other-frame "projectile" "Open matching implementation or test file in other frame.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined." t) (autoload 'projectile-toggle-between-implementation-and-test "projectile" "Toggle between an implementation file and its test file.


See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined." t) (autoload 'projectile-grep "projectile" "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

(fn &optional REGEXP ARG)" t) (autoload 'projectile-ag "projectile" "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t) (autoload 'projectile-ripgrep "projectile" "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work.

(fn SEARCH-TERM &optional ARG)" t) (autoload 'projectile-regenerate-tags "projectile" "Regenerate the project's [e|g]tags." t) (autoload 'projectile-find-tag "projectile" "Find tag in project." t) (autoload 'projectile-run-command-in-root "projectile" "Invoke `execute-extended-command' in the project's root." t) (autoload 'projectile-run-shell-command-in-root "projectile" "Invoke `shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t) (autoload 'projectile-run-async-shell-command-in-root "projectile" "Invoke `async-shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t) (autoload 'projectile-run-gdb "projectile" "Invoke `gdb' in the project's root." t) (autoload 'projectile-run-shell "projectile" "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-eshell "projectile" "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-ielm "projectile" "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-term "projectile" "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-vterm "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-vterm-other-window "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-replace "projectile" "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory and file name patterns
on which to run the replacement.

(fn &optional ARG)" t) (autoload 'projectile-replace-regexp "projectile" "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t) (autoload 'projectile-kill-buffers "projectile" "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'." t) (autoload 'projectile-save-project-buffers "projectile" "Save all project buffers." t) (autoload 'projectile-dired "projectile" "Open `dired' at the root of the project." t) (autoload 'projectile-dired-other-window "projectile" "Open `dired'  at the root of the project in another window." t) (autoload 'projectile-dired-other-frame "projectile" "Open `dired' at the root of the project in another frame." t) (autoload 'projectile-vc "projectile" "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open.

(fn &optional PROJECT-ROOT)" t) (autoload 'projectile-recentf "projectile" "Show a list of recently visited files in a project." t) (autoload 'projectile-configure-project "projectile" "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-compile-project "projectile" "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.  Per project default command can be set through
`projectile-project-compilation-cmd'.

(fn ARG)" t) (autoload 'projectile-test-project "projectile" "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-install-project "projectile" "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-package-project "projectile" "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-run-project "projectile" "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-repeat-last-command "projectile" "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited.

(fn SHOW-PROMPT)" t) (autoload 'projectile-switch-project "projectile" "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t) (autoload 'projectile-switch-open-project "projectile" "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t) (autoload 'projectile-find-file-in-directory "projectile" "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

(fn &optional DIRECTORY)" t) (autoload 'projectile-find-file-in-known-projects "projectile" "Jump to a file in any of the known projects." t) (autoload 'projectile-cleanup-known-projects "projectile" "Remove known projects that don't exist anymore." t) (autoload 'projectile-clear-known-projects "projectile" "Clear both `projectile-known-projects' and `projectile-known-projects-file'." t) (autoload 'projectile-reset-known-projects "projectile" "Clear known projects and rediscover." t) (autoload 'projectile-remove-known-project "projectile" "Remove PROJECT from the list of known projects.

(fn &optional PROJECT)" t) (autoload 'projectile-remove-current-project-from-known-projects "projectile" "Remove the current project from the list of known projects." t) (autoload 'projectile-add-known-project "projectile" "Add PROJECT-ROOT to the list of known projects.

(fn PROJECT-ROOT)" t) (autoload 'projectile-ibuffer "projectile" "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied.

(fn PROMPT-FOR-PROJECT)" t) (autoload 'projectile-commander "projectile" "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods." t) (autoload 'projectile-browse-dirty-projects "projectile" "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list.

(fn &optional CACHED)" t) (autoload 'projectile-edit-dir-locals "projectile" "Edit or create a .dir-locals.el file of the project." t) (defvar projectile-mode nil "Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.") (custom-autoload 'projectile-mode "projectile" nil) (autoload 'projectile-mode "projectile" "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

(fn &optional ARG)" t) (define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0") (register-definition-prefixes "projectile" '("??" "compilation-find-file-projectile-find-compilation-buffer" "def-projectile-commander-method" "delete-file-projectile-remove-from-cache" "project")) (provide 'projectile-autoloads)) "vertico" ((vertico-autoloads vertico-multiform vertico-indexed vertico-grid vertico vertico-quick vertico-flat vertico-suspend vertico-directory vertico-reverse vertico-mouse vertico-unobtrusive vertico-repeat vertico-buffer) (defvar vertico-mode nil "Non-nil if Vertico mode is enabled.
See the `vertico-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-mode'.") (custom-autoload 'vertico-mode "vertico" nil) (autoload 'vertico-mode "vertico" "VERTical Interactive COmpletion.

This is a global minor mode.  If called interactively, toggle the
`Vertico mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico" '("vertico-")) (defvar vertico-buffer-mode nil "Non-nil if Vertico-Buffer mode is enabled.
See the `vertico-buffer-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-buffer-mode "vertico-buffer" nil) (autoload 'vertico-buffer-mode "vertico-buffer" "Display Vertico in a buffer instead of the minibuffer.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Buffer mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-buffer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-buffer" '("vertico-buffer-")) (autoload 'vertico-directory-enter "vertico-directory" "Enter directory or exit completion with current candidate.
Exit with current input if prefix ARG is given.

(fn &optional ARG)" t) (autoload 'vertico-directory-up "vertico-directory" "Delete N names before point.

(fn &optional N)" t) (autoload 'vertico-directory-delete-char "vertico-directory" "Delete N directories or chars before point.

(fn &optional N)" t) (autoload 'vertico-directory-delete-word "vertico-directory" "Delete N directories or words before point.

(fn &optional N)" t) (autoload 'vertico-directory-tidy "vertico-directory" "Tidy shadowed file name, see `rfn-eshadow-overlay'.") (defvar vertico-flat-mode nil "Non-nil if Vertico-Flat mode is enabled.
See the `vertico-flat-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-flat-mode'.") (custom-autoload 'vertico-flat-mode "vertico-flat" nil) (autoload 'vertico-flat-mode "vertico-flat" "Flat, horizontal display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Flat mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-flat-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-flat" '("vertico-flat-")) (defvar vertico-grid-mode nil "Non-nil if Vertico-Grid mode is enabled.
See the `vertico-grid-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-grid-mode'.") (custom-autoload 'vertico-grid-mode "vertico-grid" nil) (autoload 'vertico-grid-mode "vertico-grid" "Grid display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Grid mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-grid-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-grid" '("vertico-grid-")) (defvar vertico-indexed-mode nil "Non-nil if Vertico-Indexed mode is enabled.
See the `vertico-indexed-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-indexed-mode "vertico-indexed" nil) (autoload 'vertico-indexed-mode "vertico-indexed" "Prefix candidates with indices.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Indexed mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-indexed-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-indexed" '("vertico-indexed-")) (defvar vertico-mouse-mode nil "Non-nil if Vertico-Mouse mode is enabled.
See the `vertico-mouse-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-mouse-mode "vertico-mouse" nil) (autoload 'vertico-mouse-mode "vertico-mouse" "Mouse support for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Mouse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-mouse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-mouse" '("vertico-mouse-")) (defvar vertico-multiform-mode nil "Non-nil if Vertico-Multiform mode is enabled.
See the `vertico-multiform-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-multiform-mode'.") (custom-autoload 'vertico-multiform-mode "vertico-multiform" nil) (autoload 'vertico-multiform-mode "vertico-multiform" "Configure Vertico in various forms per command.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Multiform mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-multiform-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-multiform" '("vertico-multiform-")) (autoload 'vertico-quick-jump "vertico-quick" "Jump to candidate using quick keys." t) (autoload 'vertico-quick-exit "vertico-quick" "Exit with candidate using quick keys." t) (autoload 'vertico-quick-insert "vertico-quick" "Insert candidate using quick keys." t) (register-definition-prefixes "vertico-quick" '("vertico-quick")) (autoload 'vertico-repeat-save "vertico-repeat" "Save Vertico session for `vertico-repeat'.
This function must be registered as `minibuffer-setup-hook'.") (autoload 'vertico-repeat-last "vertico-repeat" "Repeat last Vertico completion SESSION.
If called interactively from an existing Vertico session,
`vertico-repeat-last' will restore the last input and
last selected candidate for the current command.

(fn &optional SESSION)" t) (autoload 'vertico-repeat-select "vertico-repeat" "Select a Vertico session from the session history and repeat it.
If called from an existing Vertico session, you can select among
previous sessions for the current command." t) (autoload 'vertico-repeat "vertico-repeat" "Repeat last Vertico session.
If prefix ARG is non-nil, offer completion menu to select from session history.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-repeat" '("vertico-repeat-")) (defvar vertico-reverse-mode nil "Non-nil if Vertico-Reverse mode is enabled.
See the `vertico-reverse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-reverse-mode'.") (custom-autoload 'vertico-reverse-mode "vertico-reverse" nil) (autoload 'vertico-reverse-mode "vertico-reverse" "Reverse the Vertico display.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Reverse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-reverse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-reverse" '("vertico-reverse-map")) (autoload 'vertico-suspend "vertico-suspend" "Suspend the current completion session.
If the command is invoked from within the Vertico minibuffer, the
current session is suspended.  If the command is invoked from
outside the minibuffer, the active minibuffer is either selected
or the latest completion session is restored." t) (register-definition-prefixes "vertico-suspend" '("vertico-suspend--")) (defvar vertico-unobtrusive-mode nil "Non-nil if Vertico-Unobtrusive mode is enabled.
See the `vertico-unobtrusive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-unobtrusive-mode'.") (custom-autoload 'vertico-unobtrusive-mode "vertico-unobtrusive" nil) (autoload 'vertico-unobtrusive-mode "vertico-unobtrusive" "Unobtrusive display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Unobtrusive mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-unobtrusive-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-unobtrusive" '("vertico-unobtrusive--orig-count")) (provide 'vertico-autoloads)) "orderless" ((orderless orderless-autoloads) (autoload 'orderless-filter "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.

(fn STRING TABLE &optional PRED)") (autoload 'orderless-all-completions "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)") (autoload 'orderless-try-completion "orderless" "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)") (add-to-list 'completion-styles-alist '(orderless orderless-try-completion orderless-all-completions "Completion of multiple components, in any order.")) (autoload 'orderless-ivy-re-builder "orderless" "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'.

(fn STR)") (register-definition-prefixes "orderless" '("orderless-")) (provide 'orderless-autoloads)) "marginalia" ((marginalia marginalia-autoloads) (defvar marginalia-mode nil "Non-nil if Marginalia mode is enabled.
See the `marginalia-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `marginalia-mode'.") (custom-autoload 'marginalia-mode "marginalia" nil) (autoload 'marginalia-mode "marginalia" "Annotate completion candidates with richer information.

This is a global minor mode.  If called interactively, toggle the
`Marginalia mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='marginalia-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'marginalia-cycle "marginalia" "Cycle between annotators in `marginalia-annotator-registry'." t) (register-definition-prefixes "marginalia" '("marginalia-")) (provide 'marginalia-autoloads)) "consult" ((consult-autoloads consult-org consult-info consult-imenu consult-compile consult-flymake consult-xref consult-register consult-kmacro consult) (autoload 'consult-completion-in-region "consult" "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'.  Use as a value for `completion-in-region-function'.

The function can be configured via `consult-customize'.

    (consult-customize consult-completion-in-region
                       :completion-styles (basic)
                       :cycle-threshold 3)

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer

(fn START END COLLECTION &optional PREDICATE)") (autoload 'consult-outline "consult" "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate
preview.  The initial narrowing LEVEL can be given as prefix
argument.  The symbol at point is added to the future history.

(fn &optional LEVEL)" t) (autoload 'consult-mark "consult" "Jump to a marker in MARKERS list (defaults to buffer-local `mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t) (autoload 'consult-global-mark "consult" "Jump to a marker in MARKERS list (defaults to `global-mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t) (autoload 'consult-line "consult" "Search for a matching line.

Depending on the setting `consult-point-placement' the command
jumps to the beginning or the end of the first match on the line
or the line beginning.  The default candidate is the non-empty
line next to point.  This command obeys narrowing.  Optional
INITIAL input can be provided.  The search starting point is
changed if the START prefix argument is set.  The symbol at point
and the last `isearch-string' is added to the future history.

(fn &optional INITIAL START)" t) (autoload 'consult-line-multi "consult" "Search for a matching line in multiple buffers.

By default search across all project buffers.  If the prefix
argument QUERY is non-nil, all buffers are searched.  Optional
INITIAL input can be provided.  The symbol at point and the last
`isearch-string' is added to the future history.  In order to
search a subset of buffers, QUERY can be set to a plist according
to `consult--buffer-query'.

(fn QUERY &optional INITIAL)" t) (autoload 'consult-keep-lines "consult" "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  When
called from Elisp, the filtering is performed by a FILTER function.  This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn FILTER &optional INITIAL)" t) (autoload 'consult-focus-lines "consult" "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  With
optional prefix argument SHOW reveal the hidden lines.  Alternatively the
command can be restarted to reveal the lines.  When called from Elisp, the
filtering is performed by a FILTER function.  This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn FILTER &optional SHOW INITIAL)" t) (autoload 'consult-goto-line "consult" "Read line number and jump to the line with preview.

Enter either a line number to jump to the first column of the
given line or line:column in order to jump to a specific column.
Jump directly if a line number is given as prefix ARG.  The
command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'.

(fn &optional ARG)" t) (autoload 'consult-recent-file "consult" "Find recent file using `completing-read'." t) (autoload 'consult-mode-command "consult" "Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

(fn &rest MODES)" t) (autoload 'consult-yank-from-kill-ring "consult" "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'.  Additionally the Consult
version supports preview of the selected string.

(fn STRING &optional ARG)" t) (autoload 'consult-yank-pop "consult" "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'.  Additionally the Consult
version supports preview of the selected string.

(fn &optional ARG)" t) (autoload 'consult-yank-replace "consult" "Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

There exists no equivalent of this command in Emacs 28.

(fn STRING)" t) (autoload 'consult-bookmark "consult" "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing.  See the
variable `consult-bookmark-narrow' for the narrowing configuration.

(fn NAME)" t) (autoload 'consult-complex-command "consult" "Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t) (autoload 'consult-history "consult" "Insert string from HISTORY of current buffer.
In order to select from a specific HISTORY, pass the history
variable as argument.  INDEX is the name of the index variable to
update, if any.  BOL is the function which jumps to the beginning
of the prompt.  See also `cape-history' from the Cape package.

(fn &optional HISTORY INDEX BOL)" t) (autoload 'consult-isearch-history "consult" "Read a search string with completion from the Isearch history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t) (autoload 'consult-minor-mode-menu "consult" "Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t) (autoload 'consult-theme "consult" "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

(fn THEME)" t) (autoload 'consult-buffer "consult" "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources.

(fn &optional SOURCES)" t) (autoload 'consult-project-buffer "consult" "Enhanced `project-switch-to-buffer' command with support for virtual buffers.
The command may prompt you for a project directory if it is invoked from
outside a project.  See `consult-buffer' for more details." t) (autoload 'consult-buffer-other-window "consult" "Variant of `consult-buffer' which opens in other window." t) (autoload 'consult-buffer-other-frame "consult" "Variant of `consult-buffer' which opens in other frame." t) (autoload 'consult-grep "consult" "Search with `grep' for files in DIR where the content matches a regexp.

The initial input is given by the INITIAL argument.  DIR can be
nil, a directory string or a list of file/directory paths.  If
`consult-grep' is called interactively with a prefix argument,
the user can specify the directories or files to search in.
Multiple directories must be separated by comma in the
minibuffer, since they are read via `completing-read-multiple'.
By default the project directory is used if
`consult-project-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

The input string is split, the first part of the string (grep
input) is passed to the asynchronous grep process and the second
part of the string is passed to the completion-style filtering.

The input string is split at a punctuation character, which is
given as the first character of the input string.  The format is
similar to Perl-style regular expressions, e.g., /regexp/.
Furthermore command line options can be passed to grep, specified
behind --.  The overall prompt input has the form
`#async-input -- grep-opts#filter-string'.

Note that the grep input string is transformed from Emacs regular
expressions to Posix regular expressions.  Always enter Emacs
regular expressions at the prompt.  `consult-grep' behaves like
builtin Emacs search commands, e.g., Isearch, which take Emacs
regular expressions.  Furthermore the asynchronous input split
into words, each word must match separately and in any order.
See `consult--regexp-compiler' for the inner workings.  In order
to disable transformations of the grep input, adjust
`consult--regexp-compiler' accordingly.

Here we give a few example inputs:

#alpha beta         : Search for alpha and beta in any order.
#alpha.*beta        : Search for alpha before beta.
#\\(alpha\\|beta\\) : Search for alpha or beta (Note Emacs syntax!)
#word -- -C3        : Search for word, include 3 lines as context
#first#second       : Search for first, quick filter for second.

The symbol at point is added to the future history.

(fn &optional DIR INITIAL)" t) (autoload 'consult-git-grep "consult" "Search with `git grep' for files in DIR with INITIAL input.
See `consult-grep' for details.

(fn &optional DIR INITIAL)" t) (autoload 'consult-ripgrep "consult" "Search with `rg' for files in DIR with INITIAL input.
See `consult-grep' for details.

(fn &optional DIR INITIAL)" t) (autoload 'consult-find "consult" "Search for files with `find' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments.

(fn &optional DIR INITIAL)" t) (autoload 'consult-fd "consult" "Search for files with `fd' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments.

(fn &optional DIR INITIAL)" t) (autoload 'consult-locate "consult" "Search with `locate' for files which match input given INITIAL input.

The input is treated literally such that locate can take advantage of
the locate database index.  Regular expressions would often force a slow
linear search through the entire database.  The locate process is started
asynchronously, similar to `consult-grep'.  See `consult-grep' for more
details regarding the asynchronous search.

(fn &optional INITIAL)" t) (autoload 'consult-man "consult" "Search for man page given INITIAL input.

The input string is not preprocessed and passed literally to the
underlying man commands.  The man process is started asynchronously,
similar to `consult-grep'.  See `consult-grep' for more details regarding
the asynchronous search.

(fn &optional INITIAL)" t) (register-definition-prefixes "consult" '("consult-")) (autoload 'consult-compile-error "consult-compile" "Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t) (register-definition-prefixes "consult-compile" '("consult-compile--")) (autoload 'consult-flymake "consult-flymake" "Jump to Flymake diagnostic.
When PROJECT is non-nil then prompt with diagnostics from all
buffers in the current project instead of just the current buffer.

(fn &optional PROJECT)" t) (register-definition-prefixes "consult-flymake" '("consult-flymake--")) (autoload 'consult-imenu "consult-imenu" "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing.  See the variable
`consult-imenu-config', which configures the narrowing.
The symbol at point is added to the future history.

See also `consult-imenu-multi'." t) (autoload 'consult-imenu-multi "consult-imenu" "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-function' is used.  Only the buffers with the
same major mode as the current buffer are used.  See also
`consult-imenu' for more details.  In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn &optional QUERY)" t) (register-definition-prefixes "consult-imenu" '("consult-imenu-")) (autoload 'consult-info "consult-info" "Full text search through info MANUALS.

(fn &rest MANUALS)" t) (register-definition-prefixes "consult-info" '("consult-info--")) (autoload 'consult-kmacro "consult-kmacro" "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

(fn ARG)" t) (register-definition-prefixes "consult-kmacro" '("consult-kmacro--")) (autoload 'consult-org-heading "consult-org" "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered.

(fn &optional MATCH SCOPE)" t) (autoload 'consult-org-agenda "consult-org" "Jump to an Org agenda heading.

By default, all agenda entries are offered.  MATCH is as in
`org-map-entries' and can used to refine this.

(fn &optional MATCH)" t) (register-definition-prefixes "consult-org" '("consult-org--")) (autoload 'consult-register-window "consult-register" "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

(fn BUFFER &optional SHOW-EMPTY)") (autoload 'consult-register-format "consult-register" "Enhanced preview of register REG.
This function can be used as `register-preview-function'.
If COMPLETION is non-nil format the register for completion.

(fn REG &optional COMPLETION)") (autoload 'consult-register "consult-register" "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents.  For quick access
to registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in
built-in register access functions.  The command supports narrowing, see
`consult-register--narrow'.  Marker positions are previewed.  See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

(fn &optional ARG)" t) (autoload 'consult-register-load "consult-register" "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG.

(fn REG &optional ARG)" t) (autoload 'consult-register-store "consult-register" "Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally
deleting the region when a prefix ARG is given.  With a numeric prefix
ARG, store or add the number.  Otherwise store point, frameset, window or
kmacro.

(fn ARG)" t) (register-definition-prefixes "consult-register" '("consult-register-")) (autoload 'consult-xref "consult-xref" "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

(fn FETCHER &optional ALIST)") (register-definition-prefixes "consult-xref" '("consult-xref--")) (provide 'consult-autoloads)) "night-owl-theme" ((night-owl-theme-autoloads night-owl-theme) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "night-owl-theme" '("night-owl")) (provide 'night-owl-theme-autoloads)) "embark" ((embark-autoloads embark-org embark) (defun embark--record-this-command nil "Record command which opened the minibuffer.
We record this because it will be the default action.
This function is meant to be added to `minibuffer-setup-hook'." (setq-local embark--command this-command)) (add-hook 'minibuffer-setup-hook #'embark--record-this-command) (autoload 'embark-eldoc-first-target "embark" "Eldoc function reporting the first Embark target at point.
This function uses the eldoc REPORT callback and is meant to be
added to `eldoc-documentation-functions'.

(fn REPORT &rest _)") (autoload 'embark-eldoc-target-types "embark" "Eldoc function reporting the types of all Embark targets at point.
This function uses the eldoc REPORT callback and is meant to be
added to `eldoc-documentation-functions'.

(fn REPORT &rest _)") (autoload 'embark-bindings-in-keymap "embark" "Explore command key bindings in KEYMAP with `completing-read'.
The selected command will be executed.  Interactively, prompt the
user for a KEYMAP variable.

(fn KEYMAP)" t) (autoload 'embark-bindings "embark" "Explore current command key bindings with `completing-read'.
The selected command will be executed.

This shows key bindings from minor mode maps and the local
map (usually set by the major mode), but also less common keymaps
such as those from a text property or overlay, or the overriding
maps: `overriding-terminal-local-map' and `overriding-local-map'.

Additionally, if GLOBAL is non-nil (interactively, if called with
a prefix argument), this command includes global key bindings.

(fn GLOBAL)" t) (autoload 'embark-bindings-at-point "embark" "Explore all key bindings at point with `completing-read'.
The selected command will be executed.

This command lists key bindings found in keymaps specified by the
text properties `keymap' or `local-map', from either buffer text
or an overlay.  These are not widely used in Emacs, and when they
are used can be somewhat hard to discover.  Examples of locations
that have such a keymap are links and images in `eww' buffers,
attachment links in `gnus' article buffers, and the stash line
in a `vc-dir' buffer." t) (autoload 'embark-prefix-help-command "embark" "Prompt for and run a command bound in the prefix used for this command.
The prefix described consists of all but the last event of the
key sequence that ran this command.  This function is intended to
be used as a value for `prefix-help-command'.

In addition to using completion to select a command, you can also
type @ and the key binding (without the prefix)." t) (autoload 'embark-act "embark" "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate.  When called from a non-minibuffer buffer
there can multiple targets and you can cycle among them by using
`embark-cycle' (which is bound by default to the same key
binding `embark-act' is, but see `embark-cycle-key').

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

If instead you call this from outside the minibuffer, the first
ARG targets are skipped over (if ARG is negative the skipping is
done by cycling backwards) and cycling starts from the following
target.

(fn &optional ARG)" t) (autoload 'embark-act-all "embark" "Prompt the user for an action and perform it on each candidate.
The candidates are chosen by `embark-candidate-collectors'.  By
default, if `embark-select' has been used to select some
candidates, then `embark-act-all' will act on those candidates;
otherwise, if the selection is empty and `embark-act-all' is
called from a minibuffer, then the candidates are the completion
candidates.

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

(fn &optional ARG)" t) (autoload 'embark-dwim "embark" "Run the default action on the current target.
The target of the action is chosen by `embark-target-finders'.

If the target comes from minibuffer completion, then the default
action is the command that opened the minibuffer in the first
place, unless overridden by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the target's type.

See `embark-act' for the meaning of the prefix ARG.

(fn &optional ARG)" t) (autoload 'embark-become "embark" "Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using key bindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it.

If FULL is non-nil (interactively, if called with a prefix
argument), the entire minibuffer contents are used as the initial
input of the new command.  By default only the part of the
minibuffer contents between the current completion boundaries is
taken.  What this means is fairly technical, but (1) usually
there is no difference: the completion boundaries include the
entire minibuffer contents, and (2) the most common case where
these notions differ is file completion, in which case the
completion boundaries single out the path component containing
point.

(fn &optional FULL)" t) (autoload 'embark-collect "embark" "Create an Embark Collect buffer.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect\".

In Embark Collect buffers `revert-buffer' is remapped to
`embark-rerun-collect-or-export', which has slightly unusual
behavior if the buffer was obtained by running `embark-collect'
from within a minibuffer completion session.  In that case
rerunning just restarts the completion session, that is, the
command that opened the minibuffer is run again and the
minibuffer contents restored.  You can then interact normally with
the command, perhaps editing the minibuffer contents, and, if you
wish, you can rerun `embark-collect' to get an updated buffer." t) (autoload 'embark-live "embark" "Create a live-updating Embark Collect buffer.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Live\"." t) (autoload 'embark-export "embark" "Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion.

In Embark Export buffers `revert-buffer' is remapped to
`embark-rerun-collect-or-export', which has slightly unusual
behavior if the buffer was obtained by running `embark-export'
from within a minibuffer completion session.  In that case
reverting just restarts the completion session, that is, the
command that opened the minibuffer is run again and the
minibuffer contents restored.  You can then interact normally
with the command, perhaps editing the minibuffer contents, and,
if you wish, you can rerun `embark-export' to get an updated
buffer." t) (autoload 'embark-select "embark" "Add or remove the target from the current buffer's selection.
You can act on all selected targets at once with `embark-act-all'.
When called from outside `embark-act' this command will select
the first target at point." t) (register-definition-prefixes "embark" '("embark-")) (register-definition-prefixes "embark-org" '("embark-org-")) (provide 'embark-autoloads)) "embark-consult" ((embark-consult-autoloads embark-consult) (register-definition-prefixes "embark-consult" '("embark-consult-")) (provide 'embark-consult-autoloads)) "consult-company" ((consult-company consult-company-autoloads) (autoload 'consult-company "consult-company" "Interactively complete company candidates." t) (register-definition-prefixes "consult-company" '("consult-company-")) (provide 'consult-company-autoloads)) "crux" ((crux crux-autoloads) (autoload 'crux-open-with "crux" "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use.

(fn ARG)" t) (autoload 'crux-visit-term-buffer "crux" "Create or visit a terminal buffer.
If the process in that buffer died, ask to restart." t) (autoload 'crux-visit-shell-buffer "crux" "Create or visit a shell buffer.
If the process in that buffer died, ask to restart." t) (autoload 'crux-indent-rigidly-and-copy-to-clipboard "crux" "Indent region between BEGIN and END by ARG columns and copy to clipboard.

(fn BEGIN END ARG)" t) (autoload 'crux-smart-open-line-above "crux" "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode." t) (autoload 'crux-smart-open-line "crux" "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line.

(fn ARG)" t) (autoload 'crux-smart-kill-line "crux" "Kill to the end of the line and kill whole line on the next call." t) (autoload 'crux-top-join-line "crux" "Join the current line with the line beneath it." t) (autoload 'crux-kill-whole-line "crux" "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided.

(fn &optional ARG)" t) (autoload 'crux-kill-line-backwards "crux" "Kill line backwards and adjust the indentation." t) (autoload 'crux-kill-and-join-forward "crux" "If at end of line, join with following; otherwise kill line.
Passes ARG to command `kill-line' when provided.
Deletes whitespace at join.

(fn &optional ARG)" t) (autoload 'crux-move-to-mode-line-start "crux" "Move to the beginning, skipping mode specific line start regex." t) (autoload 'crux-move-beginning-of-line "crux" "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

(fn ARG)" t) (autoload 'crux-indent-defun "crux" "Indent the current defun." t) (autoload 'crux-duplicate-current-line-or-region "crux" "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated.

(fn ARG)" t) (autoload 'crux-duplicate-and-comment-current-line-or-region "crux" "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated.

(fn ARG)" t) (autoload 'crux-rename-file-and-buffer "crux" "Rename current buffer and if the buffer is visiting a file, rename it too." t) (autoload 'crux-delete-file-and-buffer "crux" "Kill the current buffer and deletes the file it is visiting." t) (autoload 'crux-copy-file-preserve-attributes "crux" "Copy the current file-visiting buffer's file to a destination.

This function prompts for the new file's location and copies it
similar to cp -p. If the new location is a directory, and the
directory does not exist, this function confirms with the user
whether it should be created. A directory must end in a slash
like `copy-file' expects. If the destination is a directory and
already has a file named as the origin file, offers to
overwrite.

If the current buffer is not a file-visiting file or the
destination is a non-existent directory but the user has elected
to not created it, nothing will be done.

When invoke with C-u, the newly created file will be visited.

(fn VISIT)" t) (autoload 'crux-view-url "crux" "Open a new buffer containing the contents of URL." t) (autoload 'crux-cleanup-buffer-or-region "crux" "Cleanup a region if selected, otherwise the whole buffer." t) (autoload 'crux-eval-and-replace "crux" "Replace the preceding sexp with its value." t) (autoload 'crux-recompile-init "crux" "Byte-compile all your dotfiles again." t) (autoload 'crux-sudo-edit "crux" "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file.

(fn &optional ARG)" t) (autoload 'crux-reopen-as-root "crux" "Find file as root if necessary.

Meant to be used as `find-file-hook'.
See also `crux-reopen-as-root-mode'.") (defvar crux-reopen-as-root-mode nil "Non-nil if Crux-Reopen-As-Root mode is enabled.
See the `crux-reopen-as-root-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `crux-reopen-as-root-mode'.") (custom-autoload 'crux-reopen-as-root-mode "crux" nil) (autoload 'crux-reopen-as-root-mode "crux" "Automatically reopen files as root if we can't write to them

as the current user.

This is a global minor mode.  If called interactively, toggle the
`Crux-Reopen-As-Root mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='crux-reopen-as-root-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'crux-insert-date "crux" "Insert a timestamp according to locale's date and time format." t) (autoload 'crux-recentf-find-file "crux" "Find a recent file using `completing-read'.
When optional argument FILTER is a function, it is used to
transform recent files before completion.

(fn &optional FILTER)" t) (autoload 'crux-recentf-find-directory "crux" "Find a recent directory using `completing-read'." t) (autoload 'crux-transpose-windows "crux" "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence.

(fn ARG)" t) (autoload 'crux-switch-to-previous-buffer "crux" "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers." t) (autoload 'crux-other-window-or-switch-buffer "crux" "Call `other-window' if more than one window is visible.
Switch to most recent buffer otherwise." t) (autoload 'crux-kill-other-buffers "crux" "Kill all buffers but the current one.
Doesn't mess with special buffers." t) (autoload 'crux-kill-buffer-truename "crux" "Kill absolute path of file visited in current buffer." t) (autoload 'crux-create-scratch-buffer "crux" "Create a new scratch buffer." t) (autoload 'crux-find-user-init-file "crux" "Edit the `user-init-file', in another window." t) (autoload 'crux-find-user-custom-file "crux" "Edit the `custom-file', in another window." t) (autoload 'crux-find-shell-init-file "crux" "Edit the shell init file in another window." t) (autoload 'crux-upcase-region "crux" "`upcase-region' when `transient-mark-mode' is on and region is active.

(fn BEG END)" t) (autoload 'crux-downcase-region "crux" "`downcase-region' when `transient-mark-mode' is on and region is active.

(fn BEG END)" t) (autoload 'crux-capitalize-region "crux" "`capitalize-region' when `transient-mark-mode' is on and region is active.

(fn BEG END)" t) (autoload 'crux-ispell-word-then-abbrev "crux" "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'.

(fn P)" t) (register-definition-prefixes "crux" '("crux-")) (provide 'crux-autoloads)) "company-wordfreq" ((company-wordfreq company-wordfreq-autoloads) (autoload 'company-wordfreq "company-wordfreq" "A company backend intended for writing texts in a human language.

The completions it proposes are words already used in the
current (or another open) buffer and matching words from a word
list file.  This word list file is supposed to be a simple list
of words ordered by the frequency the words are used in the
language.  So the first completions are words already used in the
buffer followed by matching words of the language ordered by
frequency.

See the documentation of `company-backends' for arguments COMMAND and ARG.

(fn COMMAND &optional ARG &rest IGNORED)" t) (autoload 'company-wordfreq-download-list "company-wordfreq" "Download a wordlist from FrequentWords and process it for use.

The language can be chosen from a completion list.  If the full
wordlist for the chosen language is so big, that there is a
shorter version of 50k words available, you will be prompted to
choose the short version.  Probably it is a good idea to choose
the short version as the full versions can be quite huge and
introduce latency to the completion proposals." t) (register-definition-prefixes "company-wordfreq" '("company-wordfreq-")) (provide 'company-wordfreq-autoloads)) "eros" ((eros eros-autoloads) (autoload 'eros-eval-last-sexp "eros" "Wrapper for `eval-last-sexp' that overlays results.

(fn EVAL-LAST-SEXP-ARG-INTERNAL)" t) (autoload 'eros-eval-defun "eros" "Wrapper for `eval-defun' that overlays results.

(fn EDEBUG-IT)" t) (defvar eros-mode nil "Non-nil if Eros mode is enabled.
See the `eros-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eros-mode'.") (custom-autoload 'eros-mode "eros" nil) (autoload 'eros-mode "eros" "Display Emacs Lisp evaluation results overlays.

This is a global minor mode.  If called interactively, toggle the
`Eros mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='eros-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "eros" '("eros-")) (provide 'eros-autoloads)) "adoc-mode" ((adoc-mode adoc-mode-autoloads) (autoload 'adoc-mode "adoc-mode" "Major mode for editing AsciiDoc text files.
Turning on Adoc mode runs the normal hook `adoc-mode-hook'.

(fn)" t) (add-to-list 'auto-mode-alist '("\\.a\\(?:scii\\)?doc\\'" . adoc-mode)) (register-definition-prefixes "adoc-mode" '("adoc-")) (provide 'adoc-mode-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.") (custom-autoload 'which-key-mode "which-key" nil) (autoload 'which-key-mode "which-key" "Toggle which-key-mode.

This is a global minor mode.  If called interactively, toggle the
`Which-Key mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='which-key-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'which-key-setup-side-window-right "which-key" "Apply suggested settings for side-window that opens on right." t) (autoload 'which-key-setup-side-window-right-bottom "which-key" "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise." t) (autoload 'which-key-setup-side-window-bottom "which-key" "Apply suggested settings for side-window that opens on bottom." t) (autoload 'which-key-setup-minibuffer "which-key" "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in `kbd'. REPLACEMENT
should be a cons cell of the form (STRING . COMMAND) for each
REPLACEMENT, where STRING is the replacement string and COMMAND
is a symbol corresponding to the intended command to be
replaced. COMMAND can be nil if the binding corresponds to a key
prefix. An example is

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" \\='(\"Save as\" . write-file)).

For backwards compatibility, REPLACEMENT can also be a string,
but the above format is preferred, and the option to use a string
for REPLACEMENT will eventually be removed.

(fn KEYMAP KEY REPLACEMENT &rest MORE)") (autoload 'which-key-add-key-based-replacements "which-key" "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

(which-key-add-key-based-replacements \"C-x 8\"
                                        \\='(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to `which-key-replacement-alist'.

(fn KEY-SEQUENCE REPLACEMENT &rest MORE)") (autoload 'which-key-add-major-mode-key-based-replacements "which-key" "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)") (autoload 'which-key-reload-key-sequence "which-key" "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

(fn &optional KEY-SEQ)") (autoload 'which-key-show-standard-help "which-key" "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

(fn &optional _)" t) (autoload 'which-key-show-next-page-no-cycle "which-key" "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'." t) (autoload 'which-key-show-previous-page-no-cycle "which-key" "Show previous page of keys unless on the first page, in which
case do nothing." t) (autoload 'which-key-show-next-page-cycle "which-key" "Show the next page of keys, cycling from end to beginning
after last page.

(fn &optional _)" t) (autoload 'which-key-show-previous-page-cycle "which-key" "Show the previous page of keys, cycling from beginning to end
after first page.

(fn &optional _)" t) (autoload 'which-key-show-top-level "which-key" "Show top-level bindings.

(fn &optional _)" t) (autoload 'which-key-show-major-mode "which-key" "Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

(fn &optional ALL)" t) (autoload 'which-key-show-full-major-mode "which-key" "Show all bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. " t) (autoload 'which-key-dump-bindings "which-key" "Dump bindings from PREFIX into buffer named BUFFER-NAME.

PREFIX should be a string suitable for `kbd'.

(fn PREFIX BUFFER-NAME)" t) (autoload 'which-key-undo-key "which-key" "Undo last keypress and force which-key update.

(fn &optional _)" t) (autoload 'which-key-C-h-dispatch "which-key" "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil." t) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

(fn KEYMAP)" t) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'.

(fn &optional ALL)" t) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'." t) (register-definition-prefixes "which-key" '("evil-state" "which-key-")) (provide 'which-key-autoloads)) "transient" ((transient transient-autoloads) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)") (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)") (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)") (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)") (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (register-definition-prefixes "transient" '("transient")) (provide 'transient-autoloads)) "with-editor" ((with-editor-autoloads with-editor) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

(fn &optional (ENVVAR \"EDITOR\"))" t) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

This is a global minor mode.  If called interactively, toggle the
`Shell-Command-With-Editor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='shell-command-with-editor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor")) (provide 'with-editor-autoloads)) "git-commit" ((git-commit git-commit-pkg git-commit-autoloads) (put 'git-commit-major-mode 'safe-local-variable (lambda (val) (memq val '(text-mode markdown-mode org-mode fundamental-mode git-commit-elisp-text-mode)))) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode")) (provide 'git-commit-autoloads)) "magit-section" ((magit-section-autoloads magit-section magit-section-pkg) (register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-")) (provide 'magit-section-autoloads)) "magit" ((magit-status magit-blame magit-sequence magit-subtree magit-pull magit-autoloads magit-wip magit-repos magit-sparse-checkout magit-pkg magit-apply magit-core magit-clone magit-autorevert magit-tag magit-refs magit-diff magit-margin magit-commit magit-git magit-branch magit-patch magit-fetch magit-remote magit-files magit-mode magit-merge magit-ediff magit-log magit git-rebase magit-push magit-transient magit-bisect magit-process magit-stash magit-base magit-notes magit-bookmark magit-submodule magit-worktree magit-reflog magit-gitignore magit-extras magit-bundle magit-reset) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned.") (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run \"git rebase -i\" or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running \"man git-rebase\" at the command line) for details.

(fn)" t) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp #'git-rebase-mode)) (register-definition-prefixes "git-rebase" '("git-rebase-" "magit-imenu--rebase-")) (defvar magit-define-global-key-bindings 'default "Which set of key bindings to add to the global keymap, if any.

This option controls which set of Magit key bindings, if any, may
be added to the global keymap, even before Magit is first used in
the current Emacs session.

If the value is nil, no bindings are added.

If `default', maybe add:

    C-x g     `magit-status'
    C-x M-g   `magit-dispatch'
    C-c M-g   `magit-file-dispatch'

If `recommended', maybe add:

    C-x g     `magit-status'
    C-c g     `magit-dispatch'
    C-c f     `magit-file-dispatch'

    These bindings are strongly recommended, but we cannot use
    them by default, because the \"C-c <LETTER>\" namespace is
    strictly reserved for bindings added by the user.

The bindings in the chosen set may be added when
`after-init-hook' is run.  Each binding is added if, and only
if, at that time no other key is bound to the same command,
and no other command is bound to the same key.  In other words
we try to avoid adding bindings that are unnecessary, as well
as bindings that conflict with other bindings.

Adding these bindings is delayed until `after-init-hook' is
run to allow users to set the variable anywhere in their init
file (without having to make sure to do so before `magit' is
loaded or autoloaded) and to increase the likelihood that all
the potentially conflicting user bindings have already been
added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately, when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable has no effect if `after-init-hook' has
already been run.") (custom-autoload 'magit-define-global-key-bindings "magit" t) (defun magit-maybe-define-global-key-bindings (&optional force) "See variable `magit-define-global-key-bindings'." (when magit-define-global-key-bindings (let ((map (current-global-map))) (pcase-dolist (`(,key \, def) (cond ((eq magit-define-global-key-bindings 'recommended) '(("C-x g" . magit-status) ("C-c g" . magit-dispatch) ("C-c f" . magit-file-dispatch))) ('(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))))) (when (or force (not (or (lookup-key map (kbd key)) (where-is-internal def (make-sparse-keymap) t)))) (define-key map (kbd key) def)))))) (if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook #'magit-maybe-define-global-key-bindings t)) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t) (autoload 'magit-version "magit" "Return the version of Magit currently in use.

If optional argument PRINT-DEST is non-nil, also print the used
versions of Magit, Transient, Git and Emacs to the output stream
selected by that argument.  Interactively use the echo area, or
with a prefix argument use the current buffer.  Additionally put
the output in the kill ring.

(fn &optional PRINT-DEST)" t) (register-definition-prefixes "magit" '("magit-")) (autoload 'magit-stage-buffer-file "magit-apply" "Stage all changes to the file being visited in the current buffer." t) (autoload 'magit-stage-file "magit-apply" "Read one or more files and stage all changes in those files.
With prefix argument FORCE, offer ignored files for completion.

(fn FILES &optional FORCE)" t) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t) (autoload 'magit-unstage-buffer-file "magit-apply" "Unstage all changes to the file being visited in the current buffer." t) (autoload 'magit-unstage-file "magit-apply" "Read one or more files and unstage all changes to those files.

(fn FILES)" t) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t) (register-definition-prefixes "magit-apply" '("magit-")) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.

See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-")) (autoload 'magit-emacs-Q-command "magit-base" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t) (autoload 'Info-follow-nearest-node--magit-gitman "magit-base" "

(fn FN &optional FORK)") (advice-add 'Info-follow-nearest-node :around #'Info-follow-nearest-node--magit-gitman) (advice-add 'org-man-export :around #'org-man-export--magit-gitman) (autoload 'org-man-export--magit-gitman "magit-base" "

(fn FN LINK DESCRIPTION FORMAT)") (register-definition-prefixes "magit-base" '("magit-")) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD ARGS)" t) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t) (autoload 'magit-bisect-mark "magit-bisect" "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD ARGS)" t) (register-definition-prefixes "magit-bisect" '("magit-")) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (register-definition-prefixes "magit-blame" '("magit-")) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION &optional ARGS)" t) (function-put 'magit-checkout 'interactive-only 'magit--checkout) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t) (function-put 'magit-branch-create 'interactive-only 'magit-call-git) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT &optional ARGS)" t) (function-put 'magit-branch-and-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t) (function-put 'magit-branch-or-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t) (function-put 'magit-branch-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.

If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

Require confirmation when deleting branches is dangerous in some
way.  Option `magit-no-confirm' can be customized to not require
confirmation in certain cases.  See its docstring to learn why
confirmation is required by default in certain cases or if a
prompt is confusing.

(fn BRANCHES &optional FORCE)" t) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t) (autoload 'magit-branch-configure "magit-branch" nil t) (register-definition-prefixes "magit-branch" '("magit-")) (autoload 'magit-bundle "magit-bundle" nil t) (autoload 'magit-bundle-import "magit-bundle" nil t) (autoload 'magit-bundle-create-tracked "magit-bundle" "Create and track a new bundle.

(fn FILE TAG BRANCH REFS ARGS)" t) (autoload 'magit-bundle-update-tracked "magit-bundle" "Update a bundle that is being tracked using TAG.

(fn TAG)" t) (autoload 'magit-bundle-verify "magit-bundle" "Check whether FILE is valid and applies to the current repository.

(fn FILE)" t) (autoload 'magit-bundle-list-heads "magit-bundle" "List the refs in FILE.

(fn FILE)" t) (register-definition-prefixes "magit-bundle" '("magit-")) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-sparse "magit-clone" "Clone REPOSITORY into DIRECTORY and create a sparse checkout.

(fn REPOSITORY DIRECTORY ARGS)" t) (register-definition-prefixes "magit-clone" '("magit-")) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
(git commit
--amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changed, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

(fn DATE UPDATE-AUTHOR &optional ARGS)" t) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

(fn PHASE COMMIT)" t) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (register-definition-prefixes "magit-commit" '("magit-")) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed." t) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differences between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t) (register-definition-prefixes "magit-diff" '("magit-")) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve-all "magit-ediff" "Resolve all conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(magit) Ediffing' for more information about this
and alternative commands.

(fn FILE)" t) (autoload 'magit-ediff-resolve-rest "magit-ediff" "Resolve outstanding conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(magit) Ediffing' for more information about this
and alternative commands.

(fn FILE)" t) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t) (register-definition-prefixes "magit-ediff" '("magit-ediff-")) (autoload 'magit-git-mergetool "magit-extras" nil t) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (keymap-set ido-completion-map
                          \"C-x g\" \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (keymap-set ido-common-completion-map
              \"C-x g\" \\='ido-enter-magit-status)" t) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t) (autoload 'magit-dired-am-apply-patches "magit-extras" "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository.

(fn REPO &optional ARG)" t) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t) (autoload 'magit-generate-changelog "magit-extras" "Insert ChangeLog entries into the current buffer.

The entries are generated from the diff being committed.
If prefix argument, AMENDING, is non-nil, include changes
in HEAD as well as staged changes in the diff to check.

(fn &optional AMENDING)" t) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-commit' instead of this command.

(fn FILE)" t) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

(fn REV KEYID)" t) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g., while composing a commit
message), then that repository is used.  Otherwise (e.g., while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

(fn ARG)" t) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t) (autoload 'magit-display-repository-buffer "magit-extras" "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer-other-window "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer-other-frame "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t) (register-definition-prefixes "magit-extras" '("magit-")) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t) (autoload 'magit-fetch-modules "magit-fetch" nil t) (register-definition-prefixes "magit-fetch" '("magit-")) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-file-dispatch "magit" nil t) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t) (register-definition-prefixes "magit-files" '("magit-")) (register-definition-prefixes "magit-git" '("magit-")) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t) (register-definition-prefixes "magit-gitignore" '("magit-")) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-related "magit-log" "Show log for the current branch, its upstream and its push target.
When the upstream is a local branch, then also show its own
upstream.  When `HEAD' is detached, then show log for that, the
previously checked out branch and its upstream and push-target.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\". If COMMIT is
directly on BRANCH, then show approximately
`magit-log-merged-commit-count' surrounding commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t) (autoload 'magit-shortlog "magit-log" nil t) (autoload 'magit-shortlog-since "magit-log" "Show a history summary for commits since REV.

(fn REV ARGS)" t) (autoload 'magit-shortlog-range "magit-log" "Show a history summary for commit or range REV-OR-RANGE.

(fn REV-OR-RANGE ARGS)" t) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t) (register-definition-prefixes "magit-log" '("magit-")) (register-definition-prefixes "magit-margin" '("magit-")) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t) (register-definition-prefixes "magit-merge" '("magit-")) (autoload 'magit-info "magit-mode" "Visit the Magit manual." t) (register-definition-prefixes "magit-mode" '("magit-")) (autoload 'magit-notes "magit" nil t) (register-definition-prefixes "magit-notes" '("magit-notes-")) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t) (register-definition-prefixes "magit-patch" '("magit-")) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-")) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t) (register-definition-prefixes "magit-pull" '("magit-pull-")) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t) (autoload 'magit-push-implicitly "magit-push" nil t) (autoload 'magit-push-to-remote "magit-push" nil t) (register-definition-prefixes "magit-push" '("magit-")) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t) (register-definition-prefixes "magit-reflog" '("magit-reflog-")) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t) (register-definition-prefixes "magit-refs" '("magit-")) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t) (autoload 'magit-update-default-branch "magit-remote" nil t) (autoload 'magit-remote-unshallow "magit-remote" "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec.

(fn REMOTE)" t) (autoload 'magit-remote-configure "magit-remote" nil t) (register-definition-prefixes "magit-remote" '("magit-")) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the option `magit-repository-directories' to control which
repositories are displayed." t) (register-definition-prefixes "magit-repos" '("magit-")) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t) (register-definition-prefixes "magit-reset" '("magit-reset-")) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.  `HEAD' is allowed to be detached initially.

(fn COMMITS BRANCH &optional ARGS)" t) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t) (register-definition-prefixes "magit-sequence" '("magit-")) (autoload 'magit-sparse-checkout "magit-sparse-checkout" nil t) (autoload 'magit-sparse-checkout-enable "magit-sparse-checkout" "Convert the working tree to a sparse checkout.

(fn &optional ARGS)" t) (autoload 'magit-sparse-checkout-set "magit-sparse-checkout" "Restrict working tree to DIRECTORIES.
To extend rather than override the currently configured
directories, call `magit-sparse-checkout-add' instead.

(fn DIRECTORIES)" t) (autoload 'magit-sparse-checkout-add "magit-sparse-checkout" "Add DIRECTORIES to the working tree.
To override rather than extend the currently configured
directories, call `magit-sparse-checkout-set' instead.

(fn DIRECTORIES)" t) (autoload 'magit-sparse-checkout-reapply "magit-sparse-checkout" "Reapply the sparse checkout rules to the working tree.
Some operations such as merging or rebasing may need to check out
files that aren't included in the sparse checkout.  Call this
command to reset to the sparse checkout state." t) (autoload 'magit-sparse-checkout-disable "magit-sparse-checkout" "Convert sparse checkout to full checkout.
Note that disabling the sparse checkout does not clear the
configured directories.  Call `magit-sparse-checkout-enable' to
restore the previous sparse checkout." t) (register-definition-prefixes "magit-sparse-checkout" '("magit-sparse-checkout-")) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-push "magit-stash" nil t) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.

First try \"git stash apply --index\", which tries to preserve
the index stored in the stash, if any.  This may fail because
applying the stash could result in conflicts and those have to
be stored in the index, making it impossible to also store the
stash's index there as well.

If the above failed, then try \"git stash apply\".  This fails
(with or without \"--index\") if there are any uncommitted
changes to files that are also modified in the stash.

If both of the above failed, then apply using \"git apply\".
If there are no conflicting files, use \"--3way\".  If there are
conflicting files, then using \"--3way\" requires that those
files are staged first, which may be undesirable, so prompt
the user whether to use \"--3way\" or \"--reject\".

(fn STASH)" t) (autoload 'magit-stash-pop "magit-stash" "Apply a stash to the working tree, on success remove it from stash list.

First try \"git stash pop --index\", which tries to preserve
the index stored in the stash, if any.  This may fail because
applying the stash could result in conflicts and those have to
be stored in the index, making it impossible to also store the
stash's index there as well.

If the above failed, then try \"git stash apply\".  This fails
(with or without \"--index\") if there are any uncommitted
changes to files that are also modified in the stash.

If both of the above failed, then apply using \"git apply\".
If there are no conflicting files, use \"--3way\".  If there are
conflicting files, then using \"--3way\" requires that those
files are staged first, which may be undesirable, so prompt
the user whether to use \"--3way\" or \"--reject\".

(fn STASH)" t) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from an existing STASH.
The new branch starts at the commit that was current when the
stash was created.  If the stash applies cleanly, then drop it.

(fn STASH BRANCH)" t) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH from an existing STASH.
Use the current branch or `HEAD' as the starting-point of BRANCH.
Then apply STASH, dropping it if it applies cleanly.

(fn STASH BRANCH)" t) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t) (register-definition-prefixes "magit-stash" '("magit-")) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t) (defalias 'magit #'magit-status "Begin using Magit.

This alias for `magit-status' exists for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t) (autoload 'magit-status-quick "magit-status" "Show the status of the current Git repository, maybe without refreshing.

If the status buffer of the current Git repository exists but
isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `magit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `magit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") \\='magit-status-quick)." t) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)") (register-definition-prefixes "magit-status" '("magit-")) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)") (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section.") (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash.") (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits.") (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's populated submodules." t) (register-definition-prefixes "magit-submodule" '("magit-")) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t) (register-definition-prefixes "magit-subtree" '("magit-")) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t) (autoload 'magit-tag-release "magit-tag" "Create a release tag for `HEAD'.

Assume that release tags match `magit-release-tag-regexp'.

If `HEAD's message matches `magit-release-commit-regexp', then
base the tag on the version string specified by that.  Otherwise
prompt for the name of the new tag using the highest existing
tag as initial input and leaving it to the user to increment the
desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

(fn TAG MSG &optional ARGS)" t) (register-definition-prefixes "magit-tag" '("magit-")) (register-definition-prefixes "magit-transient" '("magit-")) (defvar magit-wip-mode nil "Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.") (custom-autoload 'magit-wip-mode "magit-wip" nil) (autoload 'magit-wip-mode "magit-wip" "Save uncommitted changes to work-in-progress refs.

Whenever appropriate (i.e., when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.

See `magit-wip-after-save-local-mode' for more information on
Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip-After-Apply mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-after-apply-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip-Before-Change mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-before-change-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository.") (register-definition-prefixes "magit-wip" '("magit-")) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT &optional FORCE)" t) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t) (register-definition-prefixes "magit-worktree" '("magit-")) (provide 'magit-autoloads)) "avy" ((avy-autoloads avy) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)") (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t) (autoload 'avy-setup-default "avy" "Setup the default shortcuts.") (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t) (register-definition-prefixes "avy" '("avy-")) (provide 'avy-autoloads)) "ace-window" ((ace-window-posframe ace-window-autoloads ace-window) (autoload 'ace-select-window "ace-window" "Ace select window." t) (autoload 'ace-delete-window "ace-window" "Ace delete window." t) (autoload 'ace-swap-window "ace-window" "Ace swap window." t) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)") (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

This is a global minor mode.  If called interactively, toggle the
`Ace-Window-Display mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ace-window-display-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-")) (defvar ace-window-posframe-mode nil "Non-nil if Ace-Window-Posframe mode is enabled.
See the `ace-window-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-posframe-mode'.") (custom-autoload 'ace-window-posframe-mode "ace-window-posframe" nil) (autoload 'ace-window-posframe-mode "ace-window-posframe" "Minor mode for showing the ace window key with child frames.

This is a global minor mode.  If called interactively, toggle the
`Ace-Window-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ace-window-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "ace-window-posframe" '("ace-window-posframe-" "aw-")) (provide 'ace-window-autoloads)) "iedit" ((iedit-lib iedit iedit-rect iedit-autoloads) (autoload 'iedit-mode "iedit" "Toggle Iedit mode.
This command behaves differently, depending on the mark, point,
prefix argument and variable `iedit-transient-mark-sensitive'.

If Iedit mode is off, turn Iedit mode on.

When Iedit mode is turned on, all the occurrences of the current
region in the buffer (possibly narrowed) or a region are
highlighted.  If one occurrence is modified, the change are
propagated to all other occurrences simultaneously.

If region is not active, `iedit-default-occurrence' is called to
get an occurrence candidate, according to the thing at point.  It
might be url, email address, markup tag or current symbol(or
word).

In the above two situations, with digit prefix argument 0, only
occurrences in current function are matched.  This is good for
renaming refactoring in programming.

You can also switch to Iedit mode from isearch mode directly. The
current search string is used as occurrence.  All occurrences of
the current search string are highlighted.

With an universal prefix argument, the occurrence when Iedit mode
is turned off last time in current buffer is used as occurrence.
This is intended to recover last Iedit mode which is turned off.
If region active, Iedit mode is limited within the current
region.

With repeated universal prefix argument, the occurrence when
Iedit mode is turned off last time (might be in other buffer) is
used as occurrence.  If region active, Iedit mode is limited
within the current region.

With digital prefix argument 1, Iedit mode is limited on the
current symbol or the active region, which means just one
instance is highlighted.  This behavior serves as a start point
of incremental selection work flow.

If Iedit mode is on and region is active, Iedit mode is
restricted in the region, e.g. the occurrences outside of the
region is excluded.

If Iedit mode is on and region is active, with an universal
prefix argument, Iedit mode is restricted outside of the region,
e.g. the occurrences in the region is excluded.

Turn off Iedit mode in other situations.

Commands:
\\{iedit-mode-keymap}
Keymap used within overlays:
\\{iedit-mode-occurrence-keymap}

(fn &optional ARG)" t) (autoload 'iedit-mode-from-isearch "iedit" "Start Iedit mode using last search string as the regexp.

(fn &optional ARG)" t) (autoload 'iedit-mode-toggle-on-function "iedit" "Toggle Iedit mode on current function." t) (autoload 'iedit-execute-last-modification "iedit" "Apply last modification in Iedit mode to the current buffer or an active region.

(fn &optional ARG)" t) (register-definition-prefixes "iedit" '("iedit-")) (register-definition-prefixes "iedit-lib" '("iedit-")) (autoload 'iedit-rectangle-mode "iedit-rect" "Toggle Iedit-rect mode.

When Iedit-rect mode is on, a rectangle is started with visible
rectangle highlighting.  Rectangle editing support is based on
Iedit mechanism.

Commands:
\\{iedit-rect-keymap}

(fn &optional BEG END)" t) (register-definition-prefixes "iedit-rect" '("iedit-rect")) (provide 'iedit-autoloads)) "ivy" ((ivy elpa ivy-autoloads ivy-overlay ivy-faces colir) (register-definition-prefixes "colir" '("colir-")) (autoload 'ivy-resume "ivy" "Resume the last completion session, or SESSION if non-nil.
With a prefix arg, try to restore a recorded completion session,
if one exists.

(fn &optional SESSION)" t) (autoload 'ivy-read "ivy" "Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table, supplied for `minibuffer-completion-table'.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected. In can also be a lambda.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

PRESELECT, when non-nil, determines which one of the candidates
matching INITIAL-INPUT to select initially.  An integer stands
for the position of the desired candidate in the collection,
counting from zero.  Otherwise, use the first occurrence of
PRESELECT in the collection.  Comparison is first done with
`equal'.  If that fails, and when applicable, match PRESELECT as
a regular expression.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is re-displayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes one argument, the selected candidate. If COLLECTION is
an alist, the argument is a cons cell, otherwise it's a string.

MULTI-ACTION, when non-nil, is called instead of ACTION when
there are marked candidates. It takes the list of candidates as
its only argument. When it's nil, ACTION is called on each marked
candidate.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

EXTRA-PROPS is a plist that can be used to store
collection-specific session-specific data.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session.

(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION MULTI-ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION EXTRA-PROPS CALLER)") (autoload 'ivy-completing-read "ivy" "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value or a symbol.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored.

(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)") (defvar ivy-mode nil "Non-nil if ivy mode is enabled.
See the `ivy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.") (custom-autoload 'ivy-mode "ivy" nil) (autoload 'ivy-mode "ivy" "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

(fn &optional ARG)" t) (autoload 'ivy-switch-buffer "ivy" "Switch to another buffer." t) (autoload 'ivy-switch-view "ivy" "Switch to one of the window views stored by `ivy-push-view'." t) (autoload 'ivy-switch-buffer-other-window "ivy" "Switch to another buffer in another window." t) (register-definition-prefixes "ivy" '("ivy-" "with-ivy-window")) (register-definition-prefixes "ivy-overlay" '("ivy-")) (provide 'ivy-autoloads)) "swiper" ((swiper-autoloads swiper) (autoload 'swiper-avy "swiper" "Jump to one of the current swiper candidates with `avy'." t) (autoload 'swiper-backward "swiper" "`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t) (autoload 'swiper-thing-at-point "swiper" "`swiper' with `ivy-thing-at-point'." t) (autoload 'swiper-all-thing-at-point "swiper" "`swiper-all' with `ivy-thing-at-point'." t) (autoload 'swiper "swiper" "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t) (autoload 'swiper-all "swiper" "Run `swiper' for all open buffers.

(fn &optional INITIAL-INPUT)" t) (autoload 'swiper-isearch "swiper" "A `swiper' that's not line-based.

(fn &optional INITIAL-INPUT)" t) (autoload 'swiper-isearch-backward "swiper" "Like `swiper-isearch' but the first result is before the point.

(fn &optional INITIAL-INPUT)" t) (register-definition-prefixes "swiper" '("swiper-")) (provide 'swiper-autoloads)) "lv" ((lv-autoloads lv) (register-definition-prefixes "lv" '("lv-")) (provide 'lv-autoloads)) "hydra" ((hydra-examples hydra-ox hydra-autoloads hydra) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt 3) (register-definition-prefixes "hydra" '("defhydra" "hydra-")) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")) (register-definition-prefixes "hydra-ox" '("hydra-ox")) (provide 'hydra-autoloads)) "zoutline" ((zoutline zoutline-autoloads) (register-definition-prefixes "zoutline" '("zo-")) (provide 'zoutline-autoloads)) "lispy" ((le-js elpa lispy-pkg le-julia le-hy lispy-tags lispy-autoloads le-clojure le-racket lispy-occur le-scheme le-lisp lispy-inline le-python lispy) (register-definition-prefixes "elpa" '("straight-reload-all")) (register-definition-prefixes "le-clojure" '("lispy-")) (register-definition-prefixes "le-hy" '("lispy--")) (register-definition-prefixes "le-js" '("lispy--")) (register-definition-prefixes "le-julia" '("lispy-")) (register-definition-prefixes "le-lisp" '("lispy-")) (register-definition-prefixes "le-python" '("lispy-" "python-shell--interpreter")) (register-definition-prefixes "le-racket" '("lispy-")) (register-definition-prefixes "le-scheme" '("lispy-")) (autoload 'lispy-mode "lispy" "Minor mode for navigating and editing LISP dialects.

When `lispy-mode' is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], conditionally call commands instead of
self-inserting. The condition (called special further on) is one
of:

- the point is before \"(\"
- the point is after \")\"
- the region is active

For instance, when special, \"j\" moves down one sexp, otherwise
it inserts itself.

When special, [0-9] call `digit-argument'.

When `lispy-mode' is on, \"[\" and \"]\" move forward and
backward through lists, which is useful to move into special.

\\{lispy-mode-map}

This is a minor mode.  If called interactively, toggle the `Lispy
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lispy-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "lispy" '("ac-trigger-commands" "eval-error" "hydra-lispy-x" "lh-knight" "lispy-" "mc/cmds-to-run-" "unsupported-mode-error")) (register-definition-prefixes "lispy-inline" '("lispy-")) (autoload 'lispy-occur "lispy-occur" "Select a line within current top level sexp.
See `lispy-occur-backend' for the selection back end." t) (register-definition-prefixes "lispy-occur" '("lispy-")) (register-definition-prefixes "lispy-tags" '("lispy-" "no-semantic-support")) (provide 'lispy-autoloads)) "git-gutter" ((git-gutter-autoloads git-gutter) (autoload 'git-gutter:linum-setup "git-gutter" "Setup for linum-mode.") (autoload 'git-gutter-mode "git-gutter" "Git-Gutter mode

This is a minor mode.  If called interactively, toggle the
`Git-Gutter mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `git-gutter-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-git-gutter-mode 'globalized-minor-mode t) (defvar global-git-gutter-mode nil "Non-nil if Global Git-Gutter mode is enabled.
See the `global-git-gutter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.") (custom-autoload 'global-git-gutter-mode "git-gutter" nil) (autoload 'global-git-gutter-mode "git-gutter" "Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global Git-Gutter mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Git-Gutter mode is enabled in all buffers where `git-gutter--turn-on'
would do it.

See `git-gutter-mode' for more information on Git-Gutter mode.

(fn &optional ARG)" t) (autoload 'git-gutter "git-gutter" "Show diff information in gutter" t) (autoload 'git-gutter:toggle "git-gutter" "Toggle to show diff information." t) (register-definition-prefixes "git-gutter" '("git-gutter")) (provide 'git-gutter-autoloads)) "poet-theme" ((poet-dark-monochrome-theme poet-dark-theme poet-monochrome-theme poet-theme poet-theme-autoloads) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "poet-dark-monochrome-theme" '("poet-")) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "poet-dark-theme" '("poet-")) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "poet-monochrome-theme" '("poet-")) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "poet-theme" '("poet")) (provide 'poet-theme-autoloads)) "org-modern" ((org-modern org-modern-autoloads) (autoload 'org-modern-mode "org-modern" "Modern looks for Org.

This is a minor mode.  If called interactively, toggle the
`Org-Modern mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-modern-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'org-modern-agenda "org-modern" "Finalize Org agenda highlighting.") (put 'global-org-modern-mode 'globalized-minor-mode t) (defvar global-org-modern-mode nil "Non-nil if Global Org-Modern mode is enabled.
See the `global-org-modern-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-org-modern-mode'.") (custom-autoload 'global-org-modern-mode "org-modern" nil) (autoload 'global-org-modern-mode "org-modern" "Toggle Org-Modern mode in all buffers.
With prefix ARG, enable Global Org-Modern mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Org-Modern mode is enabled in all buffers where `org-modern--on' would
do it.

See `org-modern-mode' for more information on Org-Modern mode.

(fn &optional ARG)" t) (register-definition-prefixes "org-modern" '("org-modern-")) (provide 'org-modern-autoloads)) "centered-cursor-mode" ((centered-cursor-mode-autoloads centered-cursor-mode) (autoload 'ccm-visible-text-lines "centered-cursor-mode" "Visible text lines") (autoload 'centered-cursor-mode "centered-cursor-mode" "Makes the cursor stay vertically in a defined

position (usually centered).

This is a minor mode.  If called interactively, toggle the
`Centered-Cursor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `centered-cursor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-centered-cursor-mode 'globalized-minor-mode t) (defvar global-centered-cursor-mode nil "Non-nil if Global Centered-Cursor mode is enabled.
See the `global-centered-cursor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-centered-cursor-mode'.") (custom-autoload 'global-centered-cursor-mode "centered-cursor-mode" nil) (autoload 'global-centered-cursor-mode "centered-cursor-mode" "Toggle Centered-Cursor mode in all buffers.
With prefix ARG, enable Global Centered-Cursor mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Centered-Cursor mode is enabled in all buffers where
`centered-cursor-mode' would do it.

See `centered-cursor-mode' for more information on Centered-Cursor
mode.

(fn &optional ARG)" t) (register-definition-prefixes "centered-cursor-mode" '("animate-first-start-p" "ccm-" "recenter-sequence")) (provide 'centered-cursor-mode-autoloads)) "olivetti" ((olivetti olivetti-autoloads) (autoload 'olivetti-mode "olivetti" "Olivetti provides a nice writing environment.

Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

This is a minor mode.  If called interactively, toggle the
`Olivetti mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `olivetti-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "olivetti" '("olivetti-")) (provide 'olivetti-autoloads)) "beacon" ((beacon-autoloads beacon) (autoload 'beacon-blink "beacon" "Blink the beacon at the position of the cursor.
Unlike `beacon-blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from Lisp code." t) (defvar beacon-mode nil "Non-nil if Beacon mode is enabled.
See the `beacon-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `beacon-mode'.") (custom-autoload 'beacon-mode "beacon" nil) (autoload 'beacon-mode "beacon" "Toggle Beacon mode on or off.

This is a global minor mode.  If called interactively, toggle the
`Beacon mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='beacon-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "beacon" '("beacon-")) (provide 'beacon-autoloads)) "mixed-pitch" ((mixed-pitch-autoloads mixed-pitch) (autoload 'mixed-pitch-mode "mixed-pitch" "Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.

See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'.

This is a minor mode.  If called interactively, toggle the
`Mixed-Pitch mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `mixed-pitch-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "mixed-pitch" '("mixed-pitch-")) (provide 'mixed-pitch-autoloads)) "epl" ((epl epl-autoloads) (register-definition-prefixes "epl" '("epl-")) (provide 'epl-autoloads)) "pkg-info" ((pkg-info pkg-info-autoloads) (autoload 'pkg-info-library-original-version "pkg-info" "Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t) (autoload 'pkg-info-library-version "pkg-info" "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t) (autoload 'pkg-info-defining-library-original-version "pkg-info" "Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t) (autoload 'pkg-info-defining-library-version "pkg-info" "Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t) (autoload 'pkg-info-package-version "pkg-info" "Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

(fn PACKAGE &optional SHOW)" t) (autoload 'pkg-info-version-info "pkg-info" "Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

(fn LIBRARY &optional PACKAGE SHOW)" t) (register-definition-prefixes "pkg-info" '("pkg-info-")) (provide 'pkg-info-autoloads)) "let-alist" ((let-alist let-alist-autoloads) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one.  You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function 1) (register-definition-prefixes "let-alist" '("let-alist--")) (provide 'let-alist-autoloads)) "flycheck" ((flycheck-autoloads flycheck-buttercup flycheck-ert flycheck) (autoload 'flycheck-manual "flycheck" "Open the Flycheck manual." t) (autoload 'flycheck-mode "flycheck" "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is ‘toggle’; disable the mode otherwise.

(fn &optional ARG)" t) (put 'global-flycheck-mode 'globalized-minor-mode t) (defvar global-flycheck-mode nil "Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.") (custom-autoload 'global-flycheck-mode "flycheck" nil) (autoload 'global-flycheck-mode "flycheck" "Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Flycheck mode is enabled in all buffers where `flycheck-mode-on-safe'
would do it.

See `flycheck-mode' for more information on Flycheck mode.

(fn &optional ARG)" t) (autoload 'flycheck-define-error-level "flycheck" "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

(fn LEVEL &rest PROPERTIES)") (function-put 'flycheck-define-error-level 'lisp-indent-function 1) (autoload 'flycheck-define-command-checker "flycheck" "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

(fn SYMBOL DOCSTRING &rest PROPERTIES)") (function-put 'flycheck-define-command-checker 'lisp-indent-function 1) (function-put 'flycheck-define-command-checker 'doc-string-elt 2) (autoload 'flycheck-def-config-file-var "flycheck" "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-config-file-var 'lisp-indent-function 3) (autoload 'flycheck-def-option-var "flycheck" "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-option-var 'lisp-indent-function 3) (function-put 'flycheck-def-option-var 'doc-string-elt 4) (autoload 'flycheck-define-checker "flycheck" "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t) (function-put 'flycheck-define-checker 'lisp-indent-function 1) (function-put 'flycheck-define-checker 'doc-string-elt 2) (register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors")) (register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list")) (register-definition-prefixes "flycheck-ert" '("flycheck-er")) (provide 'flycheck-autoloads)) "consult-flyspell" ((consult-flyspell-autoloads consult-flyspell) (autoload 'consult-flyspell "consult-flyspell" "Display all misspelled words in the current buffer.
If called with prefix U  or `consult-flyspell-always-check-buffer' set to t
will check buffer with `flyspell-buffer' first.

(fn &optional U)" t) (register-definition-prefixes "consult-flyspell" '("consult-flyspell-")) (provide 'consult-flyspell-autoloads)) "posframe" ((posframe-benchmark posframe-autoloads posframe) (autoload 'posframe-workable-p "posframe" "Test posframe workable status.") (autoload 'posframe-show "posframe" "Pop up a posframe to show STRING at POSITION.

 (1) POSITION

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

 (2) POSHANDLER

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-start xxx
   :parent-window-end xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :mouse-x xxx
   ;mouse-y xxx
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.

The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-top-left-or-right-other-corner'
6.  `posframe-poshandler-frame-bottom-center'
7.  `posframe-poshandler-frame-bottom-left-corner'
8.  `posframe-poshandler-frame-bottom-right-corner'
9.  `posframe-poshandler-window-center'
10.  `posframe-poshandler-window-top-center'
11. `posframe-poshandler-window-top-left-corner'
12. `posframe-poshandler-window-top-right-corner'
13. `posframe-poshandler-window-bottom-center'
14. `posframe-poshandler-window-bottom-left-corner'
15. `posframe-poshandler-window-bottom-right-corner'
16. `posframe-poshandler-point-top-left-corner'
17. `posframe-poshandler-point-bottom-left-corner'
18. `posframe-poshandler-point-bottom-left-corner-upward'
19. `posframe-poshandler-point-window-center'
20. `posframe-poshandler-point-frame-center'

 (3) POSHANDLER-EXTRA-INFO

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: `info', it will *OVERRIDE* the
exist key in `info'.

 (4) BUFFER-OR-NAME

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

buffer name can prefix with space, for example \" *mybuffer*\", so
the buffer name will hide for ibuffer and `list-buffers'.

 (5) NO-PROPERTIES

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

 (6) HEIGHT, MAX-HEIGHT, MIN-HEIGHT, WIDTH, MAX-WIDTH and MIN-WIDTH

These arguments are specified in the canonical character width
and height of posframe, more details can be found in docstring of
function `fit-frame-to-buffer',

 (7) LEFT-FRINGE and RIGHT-FRINGE

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

 (8) BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and BORDER-COLOR, but do not suggest to use for the
reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

 (9) FONT, FOREGROUND-COLOR and BACKGROUND-COLOR

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

 (10) CURSOR and WINDOW-POINT

By default, cursor is not showed in posframe, user can let cursor
showed with this argument help by set its value to a `cursor-type'.

When cursor need to be showed in posframe, user may need to set
WINDOW-POINT to the point of BUFFER, which can let cursor showed
at this point.

 (11) RESPECT-HEADER-LINE and RESPECT-MODE-LINE

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

 (12) INITIALIZE

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

 (13) LINES-TRUNCATE

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

 (14) OVERRIDE-PARAMETERS

OVERRIDE-PARAMETERS is very powful, *all* the valid frame parameters
used by posframe's frame can be overridden by it.

NOTE: some `posframe-show' arguments are not frame parameters, so they
can not be overrided by this argument.

 (15) TIMEOUT

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

 (15) REFRESH

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

 (17) ACCEPT-FOCUS

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

 (18) HIDEHANDLER

HIDEHANDLER is a function, when it return t, posframe will be
hide, this function has a plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'

 (19) REFPOSHANDLER

REFPOSHANDLER is a function, a reference position (most is
top-left of current frame) will be returned when call this
function.

when it is nil or it return nil, child-frame feature will be used
and reference position will be deal with in Emacs.

The user case I know at the moment is let ivy-posframe work well
in EXWM environment (let posframe show on the other application
window).

         DO NOT USE UNLESS NECESSARY!!!

An example parent frame poshandler function is:

1. `posframe-refposhandler-xwininfo'

 (19) Others

You can use `posframe-delete-all' to delete all posframes.

(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER POSHANDLER-EXTRA-INFO WIDTH HEIGHT MAX-WIDTH MAX-HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE BORDER-WIDTH BORDER-COLOR INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT CURSOR WINDOW-POINT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO LINES-TRUNCATE OVERRIDE-PARAMETERS TIMEOUT REFRESH ACCEPT-FOCUS HIDEHANDLER REFPOSHANDLER &allow-other-keys)") (autoload 'posframe-hide-all "posframe" "Hide all posframe frames." t) (autoload 'posframe-delete-all "posframe" "Delete all posframe frames and buffers." t) (register-definition-prefixes "posframe" '("posframe-")) (autoload 'posframe-benchmark "posframe-benchmark" "Benchmark tool for posframe." t) (register-definition-prefixes "posframe-benchmark" '("posframe-benchmark-alist")) (provide 'posframe-autoloads)) "company-posframe" ((company-posframe-autoloads company-posframe) (defvar company-posframe-mode nil "Non-nil if Company-Posframe mode is enabled.
See the `company-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-posframe-mode'.") (custom-autoload 'company-posframe-mode "company-posframe" nil) (autoload 'company-posframe-mode "company-posframe" "company-posframe minor mode.

This is a global minor mode.  If called interactively, toggle the
`Company-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='company-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "company-posframe" '("company-posframe-")) (provide 'company-posframe-autoloads)) "yasnippets" ((yasnippets)) "frame-local" ((frame-local frame-local-autoloads) (register-definition-prefixes "frame-local" '("frame-local-")) (provide 'frame-local-autoloads)) "company-box" ((company-box-doc company-box-autoloads company-box-icons company-box) (autoload 'company-box-mode "company-box" "Company-box minor mode.

This is a minor mode.  If called interactively, toggle the
`company-box mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `company-box-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "company-box" '("company-box-")) (register-definition-prefixes "company-box-doc" '("company-box-")) (register-definition-prefixes "company-box-icons" '("company-box-icons-")) (provide 'company-box-autoloads)) "hyperbole" ((hyperbole hmouse-sh hvar hibtypes hib-doc-id hui-dired-sidebar hib-debbugs hui-window hib-kbd hmouse-info hact hargs hypb-ert hui-register hinit hsys-youtube hmouse-key hmoccur hload-path hypb hrmail hsys-org-roam hyperbole-autoloads hsys-www hpath hmh set hib-social hhist hui-mini hui-menu hgnus hyrolo-menu hmail hui-treemacs hypb-maintenance hywconfig hactypes hui-mouse hui hvm hui-jmenu hyrolo hyrolo-logic hsmail hmouse-drv hyrolo-demo hversion hbdata htz hui-em-but hmouse-mod hsys-org hbmap hbut hui-select hycontrol hsettings hmouse-tag) (autoload 'htype:symbol "hact" "Return possibly new Hyperbole type symbol composed from TYPE and TYPE-CATEGORY.
TYPE and TYPE-CATEGORY are both symbols.  TYPE-CATEGORY must be one of
`actypes' or `ibtypes'; if not, return nil.

(fn TYPE TYPE-CATEGORY)") (register-definition-prefixes "hact" '("act" "defact" "hact" "hrule:action" "htype:" "sym")) (register-definition-prefixes "hactypes" '("annot-bib" "completion" "display-" "eval-elisp" "exec-" "hactypes:link-to-file-interactively" "hyp-" "link-to-" "man-show" "rfc-toc" "text-toc")) (register-definition-prefixes "hargs" '("hargs:")) (register-definition-prefixes "hbdata" '("hbdata:")) (register-definition-prefixes "hbmap" '("hbmap:")) (autoload 'hbut:modify-syntax "hbut" "Modify syntactic character pairs in syntax tables.
Modify `hbut:syntax-table' and `help-mode-syntax-table'.  For use
with implicit button activations.") (register-definition-prefixes "hbut" '("defal" "defi" "ebut:" "gbut:" "hattr:" "hbut:" "ibtype:" "ibut:" "map-")) (autoload 'Gnus-init "hgnus" "Initialize Hyperbole support for Gnus Usenet news reading." t) (register-definition-prefixes "hgnus" '("lnews:to" "rnews:")) (register-definition-prefixes "hhist" '("*hhist*" "hhist:")) (register-definition-prefixes "hib-debbugs" '("debbugs-" "smart-debbugs-gnu")) (register-definition-prefixes "hib-doc-id" '("doc-id" "link-to-doc")) (register-definition-prefixes "hib-kbd" '("kbd-key")) (register-definition-prefixes "hib-social" '("git" "hibtypes-" "social-reference")) (register-definition-prefixes "hibtypes" '("Info-node" "action" "annot-bib" "completion" "cscope" "ctags" "debugger-source" "dir-summary" "eli" "etags" "glink" "gnus-push-button" "grep-msg" "hib-python-traceback" "hlink" "hyp-" "id-cflow" "ilink" "ipython-stack-frame" "mail-address" "man-apropos" "markdown-" "org-" "pat" "python-tb-previous-line" "rfc" "ripgrep-msg" "tex")) (autoload 'hyperb:init-menubar "hinit" "Add a pulldown menu for Hyperbole after Emacs is initialized." t) (autoload 'hui-menu-remove "hinit" "Remove MENU-SYM from menubars generated by optional KEYMAP or the `global-map'.

(fn MENU-SYM &optional KEYMAP)" nil t) (register-definition-prefixes "hinit" '("hyperb:")) (register-definition-prefixes "hload-path" '("hload-path--" "hyperb:")) (autoload 'hmail:compose "hmail" "Compose mail with ADDRESS and evaluation of EXPR.
Optional SUBJECT and HELP message may also be given.

(fn ADDRESS EXPR &optional SUBJECT HELP)" t) (autoload 'hmail:msg-narrow "hmail" "Narrows buffer to displayable part of current message.
Its displayable part begins at optional MSG-START and ends at or before
MSG-END.

(fn &optional MSG-START MSG-END)") (register-definition-prefixes "hmail" '("hmail:" "hnews:" "rmail:")) (autoload 'Mh-init "hmh" "Initialize Hyperbole support for Mh mail reading." t) (register-definition-prefixes "hmh" '("Mh-" "hmh--")) (register-definition-prefixes "hmoccur" '("moccur")) (autoload 'hkey-ace-window-setup "hmouse-drv" "Bind optional keyboard KEY and setup display of items specified by short ids.

The ace-window package, (see \"https://elpa.gnu.org/packages/ace-window.html\"),
assigns short ids to each Emacs window and lets you jump to or
operate upon a specific window by giving its letter.  Hyperbole
can insert an operation into ace-window that allows you to
display items such as dired or buffer menu items in a specific
window.

To enable this feature, in your Emacs initialization file after
Hyperbole is initialized, if you already have a key bound for
ace-window, then call:

 (hkey-ace-window-setup)

otherwise, choose a binding like {M-o} and send it to the same
function to bind it:

 (hkey-ace-window-setup \"\357\")

Then whenever point is on an item you want displayed in another
window, use {M-o i <id-of-window-to-display-item-in>} and watch the
magic happen.

(fn &optional KEY)") (autoload 'hkey-drag "hmouse-drv" "Emulate Smart Mouse Key drag from the selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  The drag action determines the final selected
window.

Optional prefix arg non-nil means emulate Assist Key rather than the
Action Key.

Works only when running under a window system, not from a dumb terminal.

(fn RELEASE-WINDOW)" t) (autoload 'hkey-drag-stay "hmouse-drv" "Emulate Smart Mouse Key drag from selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  After the drag, the selected window remains the same
as it was before the drag.

Works only when running under a window system, not from a dumb terminal.

(fn RELEASE-WINDOW)") (autoload 'hkey-drag-item "hmouse-drv" "Emulate Smart Mouse Key drag from item in a selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  RELEASE-WINDOW is left selected unless point is not
on an item, in which case, an error is signalled.

Optional prefix arg non-nil means emulate Assist Key rather than the
Action Key.

Works only when running under a window system, not from a dumb terminal.

(fn RELEASE-WINDOW)" t) (autoload 'hkey-drag-to "hmouse-drv" "Emulate Smart Mouse Key drag from a selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  If an item is dragged to RELEASE-WINDOW, then
RELEASE-WINDOW is selected; otherwise, the drag action determines
the selected window.  If no drag has taken place, then the
selected window's buffer is displayed in RELEASE-WINDOW and that
becomes the selected window.

Optional prefix arg non-nil means emulate Assist Key rather than the
Action Key.

Works only when running under a window system, not from a dumb terminal.

(fn RELEASE-WINDOW)" t) (autoload 'hkey-link "hmouse-drv" "Return a list of the selected window (where depressed) and the RELEASE-WINDOW.

(fn RELEASE-WINDOW)") (autoload 'hkey-replace "hmouse-drv" "Grab the buffer from RELEASE-WINDOW and place it into the current window.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  The selected window does not change.

(fn RELEASE-WINDOW)" t) (autoload 'hkey-swap "hmouse-drv" "Swap the buffer from the selected window with that of TO-WINDOW.
When called interactively the TO-WINDOW is chosen via ace-window.  Leave
TO-WINDOW as the selected window.

(fn TO-WINDOW)" t) (autoload 'hkey-throw "hmouse-drv" "Throw a thing to display in RELEASE-WINDOW.
Throw one of:
 - the active (highlighted) region,
 - a displayable item at point or
 - the current buffer.
With optional prefix arg THROW-REGION-FLAG, throw the current region
even if not active.
The selected window does not change.

(fn RELEASE-WINDOW &optional THROW-REGION-FLAG)" t) (autoload 'hkey-window-link "hmouse-drv" "Create a but in the selected window, linked to point in RELEASE-WINDOW.
RELEASE-WINDOW is interactively selected via the `ace-window' command.
The selected window does not change.

With no prefix argument, create an explicit button.
With a C-u \\='(4) prefix argument, create an unnamed implicit button.
With a M-1 prefix argument, create an named implicit button.

(fn RELEASE-WINDOW)" t) (autoload 'hkey-buffer-to "hmouse-drv" "Display buffer from FROM-WINDOW in TO-WINDOW.
When interactive use ace-window to choose FROM-WINDOW and
TO-WINDOW.  The selected window does not change.

(fn FROM-WINDOW TO-WINDOW)" t) (autoload 'hkey-swap-buffers "hmouse-drv" "Swap buffer from FROM-WINDOW with buffer of TO-WINDOW.
When interactive use ace-window to choose FROM-WINDOW and
TO-WINDOW.  Leave TO-WINDOW as the selected window.

(fn FROM-WINDOW TO-WINDOW)" t) (autoload 'hmouse-click-to-drag "hmouse-drv" "Mouse click on start and end windows for use with `hkey-drag'.
Emulate Smart Mouse Key drag from start window to end window.
The drag action determines the final selected window." t) (autoload 'hmouse-click-to-drag-stay "hmouse-drv" "Mouse click on start and end windows for use with `hkey-drag-stay'.
Emulate Smart Mouse Key drag from start window to end window.
The selected window does not change." t) (autoload 'hmouse-click-to-drag-item "hmouse-drv" "Mouse click on start and end windows for use with `hkey-drag-item'.
Emulate {M-o i} from start window to end window.
After the drag, the end window is the selected window." t) (autoload 'hmouse-click-to-drag-to "hmouse-drv" "Mouse click on start and end windows for use with `hkey-drag-to'.
Emulate Smart Mouse Key drag from start window to end window.
After the drag, the end window is the selected window." t) (autoload 'hmouse-click-to-replace "hmouse-drv" "Mouse click on start and end windows for use with `hkey-replace'.
Replace the buffer in start window with the buffer in end window.
The selected window does not change." t) (autoload 'hmouse-click-to-swap "hmouse-drv" "Mouse click on start and end windows for use with `hkey-swap'.
Swap the buffer in start window with the buffer in end window.
Leave the end window selected." t) (autoload 'hmouse-click-to-throw "hmouse-drv" "Mouse click on start and end windows for use with `hkey-throw'.
Throw either a displayable item at start window's point or its current
buffer to the end window.  The selected window does not change." t) (autoload 'hkey-buffer-move-left "hmouse-drv" "Swap the current buffer with the one on its left, if any; otherwise, do nothing." t) (autoload 'hkey-buffer-move-right "hmouse-drv" "Swap the current buffer with the one on its right, if any; otherwise do nothing." t) (autoload 'hkey-buffer-move-down "hmouse-drv" "Swap the current buffer with the one below it, if any; otherwise, do nothing." t) (autoload 'hkey-buffer-move-up "hmouse-drv" "Swap the current buffer with the one on above it, if any; otherwise, do nothing." t) (autoload 'hkey-help-hide "hmouse-drv" "Optionally KILL current buffer (default is bury) and quit WINDOW.
Restore frame to configuration prior to help buffer display.
Point must be in a help buffer.  See `hkey-quit-window' for additional
details.

(fn &optional KILL WINDOW)" t) (autoload 'hkey-help-show "hmouse-drv" "Save prior window configuration if BUFFER displays help.  Display BUFFER.

With optional second arg CURRENT-WINDOW non-nil, force display of buffer within
the current window.  By default, it is displayed according to the setting of
`hpath:display-where'.

(fn &optional BUFFER CURRENT-WINDOW)") (register-definition-prefixes "hmouse-drv" '("action-" "assist-" "hkey-" "hmouse-" "mouse-drag-mode-line" "quit-window" "smart-scroll-")) (autoload 'Info-read-index-item-name "hmouse-info" "Read an Info index item name with completion, prompting with PROMPT.
An index item name can have the form \"itemname\", referring to an index
item in the current Info file, or \"(filename)itemname\", referring to
an item in filename.  \"(filename)\" is a short format to go to
the Top node in filename.  Signal an error if a filename without an
index is given.

(fn PROMPT)") (autoload 'smart-info "hmouse-info" "Walks through Info documentation networks using one key or mouse key.

If key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the desired node is found;
 (3) the File entry of a Node Header (first line),
       the `Top' node within that file is found;
 (4) at the end of the current node, the Next node is found (this will
       descend subtrees if the function `Info-global-next' is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled up one windowful.

Returns t if key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil." t) (autoload 'smart-info-assist "hmouse-info" "Walk through Info documentation using one assist-key or mouse assist-key.

If assist-key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the last node in the history list is found;
 (3) the File entry of a Node Header (first line),
       the `DIR' root-level node is found;
 (4) at the end of the current node, the Previous node is found (this will
       return from subtrees if the function `Info-global-prev' is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled down one windowful.

Returns t if assist-key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil." t) (autoload 'Info-handle-in-note "hmouse-info" "Follows an Info cross-reference.
If point is within the first line of an Info note (cross-reference), follows
cross-reference and returns t; otherwise returns nil.") (autoload 'Info-current-filename-sans-extension "hmouse-info" "Return the filename for the current Info node, if any.
Filename is returned without directory or file extension.
This works regardless of the current buffer.") (autoload 'Info-menu-item-at-p "hmouse-info" "Return the name of the Info menu item at point, or nil if none.") (autoload 'Info-note-at-p "hmouse-info" "Return the name of the Info cross-reference note at point, or nil if none.") (register-definition-prefixes "hmouse-info" '("Info-")) (register-definition-prefixes "hmouse-key" '("hmouse-")) (defvar hmouse-mod-mode nil "Non-nil if Hmouse-Mod mode is enabled.
See the `hmouse-mod-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hmouse-mod-mode'.") (custom-autoload 'hmouse-mod-mode "hmouse-mod" nil) (autoload 'hmouse-mod-mode "hmouse-mod" "Toggle use of Smart Keys as Control- and Meta- modifiers (Hmouse Modifier mode).
With a prefix argument ARG, enable Hmouse Mod mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

If the Action Key is held down while alpha characters are typed,
they are translated into Control keys instead.  The Assist Key
translates them into Meta keys.  When both Smart Keys are depressed,
Control-Meta keys are produced.  The commands bound to the
characters produced are then run.

Hmouse Modifier mode is a global minor mode.  It does not affect
unmodified keys.  Normal Smart Key operations work with this
mode, if no other key is pressed while a Smart Key is depressed.

(fn &optional ARG)" t) (register-definition-prefixes "hmouse-mod" '("hmouse-mod-")) (register-definition-prefixes "hmouse-sh" '("hmouse-")) (autoload 'smart-asm-at-tag-p "hmouse-tag" "Return assembly tag name that point is within, else nil.

(fn &optional NO-FLASH)") (autoload 'smart-c++ "hmouse-tag" "Jumps to the definition of optional C++ IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C++ tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `c++-to-definition' for the behavior of this
function when the OO-Browser has been loaded.
Otherwise:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-path'
     and `smart-c-include-path';
 (2) on a C++ identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories;
 (3) if `smart-c-use-lib-man' is non-nil, the C++ identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed.

(fn &optional IDENTIFIER NEXT)" t) (autoload 'smart-c++-tag "hmouse-tag" "

(fn &optional IDENTIFIER NEXT)") (autoload 'smart-c-at-tag-p "hmouse-tag" "Return C tag name that point is within, else nil.

(fn &optional NO-FLASH)") (autoload 'smart-cc-mode-initialize "hmouse-tag" "Load and initialize cc-mode if possible and always return nil.") (autoload 'smart-fortran-at-tag-p "hmouse-tag" "Return Fortran tag name that point is within, else nil.

(fn &optional NO-FLASH)") (autoload 'smart-java "hmouse-tag" "Jumps to the definition of optional Java IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Java tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `smart-java-oo-browser' for the behavior of this
function when the OO-Browser has been loaded.
Otherwise:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-path';
 (3) on a Java identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.

(fn &optional IDENTIFIER NEXT)" t) (autoload 'smart-java-tag "hmouse-tag" "

(fn &optional IDENTIFIER NEXT)") (autoload 'smart-java-at-tag-p "hmouse-tag" "Return Java tag name that point is within, else nil.

(fn &optional NO-FLASH)") (autoload 'smart-javascript "hmouse-tag" "Jump to the definition of optional JavaScript IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching JavaScript tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If on a JavaScript identifier, the identifier definition is displayed,
assuming the identifier is found within an `etags' generated tag file
in the current directory or any of its ancestor directories.

(fn &optional IDENTIFIER NEXT)" t) (autoload 'smart-javascript-at-tag-p "hmouse-tag" "Return JavaScript tag name that point is within, else nil.

(fn &optional NO-FLASH)") (defconst smart-lisp-identifier-first-char-regexp "[-<>*a-zA-Z]" "Regexp matching the first character of a Lisp identifier.") (defconst smart-lisp-identifier-chars "-_:/*+=%$&?!<>a-zA-Z0-9~^" "Regexp matching a valid char in a Lisp identifier except the first char.
Excludes character matching square brackets, so may be used with
skip-characters-forward/backward.") (defconst smart-lisp-identifier (concat smart-lisp-identifier-first-char-regexp "[" smart-lisp-identifier-chars "]*") "Regexp matching a Lisp identifier.") (autoload 'smart-lisp-mode-p "hmouse-tag" "Return t if in a mode which use Lisp symbols.") (autoload 'smart-objc "hmouse-tag" "Jump to the definition of optional Objective-C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Objective-C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `smart-objc-oo-browser' for the behavior of this
function when the OO-Browser has been loaded.
Otherwise:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `objc-cpp-include-path' and
     `objc-include-path';
 (2) on an Objective-C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories;
 (3) if `smart-c-use-lib-man' is non-nil, the Objective-C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed.

(fn &optional IDENTIFIER NEXT)" t) (autoload 'smart-objc-tag "hmouse-tag" "

(fn &optional IDENTIFIER NEXT)") (autoload 'smart-python "hmouse-tag" "Jumps to the definition of optional Python IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Python tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `smart-python-jedi-to-definition-p' for the
behavior when the Jedi python identifier server is in use.

See the documentation for `smart-python-oo-browser' for the behavior of this
function when the OO-Browser has been loaded.

Otherwise, on a Python identifier, the identifier definition is displayed,
assuming the identifier is found within an `etags' generated tag file
in the current directory or any of its ancestor directories.

(fn &optional IDENTIFIER NEXT)" t) (autoload 'smart-python-tag "hmouse-tag" "

(fn &optional IDENTIFIER NEXT)") (autoload 'smart-python-at-tag-p "hmouse-tag" "Return Python tag name that point is within, else nil.

(fn &optional NO-FLASH)") (autoload 'smart-tags-file-path "hmouse-tag" "Expand relative FILE name by looking it up within appropriate tags files.
Return FILE unchanged if it exists relative to the current directory or
cannot be expanded via a tags file.

(fn FILE)") (autoload 'smart-tags-file-list "hmouse-tag" "Return tag files list for optional CURR-DIR-OR-FILENAME or `default-directory'.
Optional NAME-OF-TAGS-FILE is the literal filename (no directory) for which
to look.  If no tags file is found, an error is signaled.

(fn &optional CURR-DIR-OR-FILENAME NAME-OF-TAGS-FILE)") (register-definition-prefixes "hmouse-tag" '("smart-")) (defvar hpath:posix-mount-point-to-mswindows-alist nil "Automatically set alist of (posix-mount-point . window-path-prefix) elements.
Used to expand posix mount points to Windows UNC paths during
posix-to-mswindows conversion.") (autoload 'hpath:mswindows-to-posix "hpath" "Convert a MSWindows PATH to a Posix-style path or return the path unchanged.
If path begins with an MSWindows drive letter, prefix the
converted path with the value of `hpath:mswindows-mount-prefix'.

(fn PATH)" t) (autoload 'hpath:posix-to-mswindows "hpath" "Convert a Posix-style PATH to an MSWindows path or return the path unchanged.
If path begins with an optional mount prefix,
`hpath:mswindows-mount-prefix', followed by an MSWindows drive
letter, remove the mount prefix.

(fn PATH)" t) (autoload 'hpath:substitute-posix-or-mswindows-at-point "hpath" "If point is in a Posix or MSWindows path, change the path to the other type." t) (autoload 'hpath:substitute-posix-or-mswindows "hpath" "Change a recognizable Posix or MSWindows PATH to the other type of path.

(fn PATH)") (autoload 'hpath:cache-mswindows-mount-points "hpath" "Cache valid MSWindows mounts when under a non-MSWindows OS, e.g. WSL.
Mount points are cached in `directory-abbrev-alist'.
Call this function manually if mount points change after Hyperbole is loaded." t) (autoload 'hpath:display-buffer "hpath" "Display and select BUFFER at optional DISPLAY-WHERE or at `hpath:display-where'.
BUFFER must be a buffer or a buffer name.

See the documentation of `hpath:display-buffer-alist' for valid
values of DISPLAY-WHERE.  Return the window in which the buffer
is displayed or nil if not displayed because BUFFER is invalid.

(fn BUFFER &optional DISPLAY-WHERE)" t) (autoload 'hpath:find-file-urls-mode "hpath" "Toggle use of ftp and www Urls as arguments to `find-file' commands.
With optional prefix ARG, enable this feature if ARG is positive or turn it
off otherwise.

(fn &optional ARG)" t) (register-definition-prefixes "hpath" '("hpath:" "hyperb:substitute-in-file-name" "substitute-in-file-name")) (autoload 'Rmail-init "hrmail" "Initialize Hyperbole support for Rmail mail reading." t) (register-definition-prefixes "hrmail" '("Rmail-" "hrmail--")) (autoload 'hyperbole-toggle-messaging "hsettings" "Toggle Hyperbole support for explicit buttons in mail and news buffers.
Toggle the boolean variable `inhibit-hyperbole-messaging’ and either
add hooks (nil value) or remove them (t value).

With optional prefix ARG > 0, enable support.  If ARG <= 0,
disable/inhibit support.

(fn &optional ARG)" t) (register-definition-prefixes "hsettings" '("hkey-always-display-menu" "hmouse-middle-flag" "hui:but-flash" "hyperbole-" "inhibit-hyperbole-messaging" "smart-scroll-proportional")) (register-definition-prefixes "hsmail" '("mail-yank-original" "message--yank-original-internal" "smail:")) (autoload 'hsys-org-meta-return-shared-p "hsys-org" "Return non-nil if hyperbole-mode is active and shares the org-meta-return key.") (autoload 'hsys-org-meta-return "hsys-org" "Call `org-meta-return' with the numeric value of any prefix arg when given." t) (defvar hsys-org-enable-smart-keys 'unset "This applies in Org major/minor modes only when `hyperbole-mode' is active.
If set to \\='unset prior to loading Hyperbole, then Hyperbole
initialization will set its value.

The following table shows what the Smart Keys do in various contexts
with different settings of this option.  For example, a nil value makes
{M-RET} operate as it normally does within Org mode contexts.

|---------+-------------------+------------------+----------+------------------|
| Setting | Smart Key Context | Hyperbole Button | Org Link | Fallback Command |
|---------+-------------------+------------------+----------+------------------|
| buttons | Ignore            | Activate         | Activate | org-meta-return  |
| nil     | Ignore            | Ignore           | Ignore   | org-meta-return  |
| t       | Activate          | Activate         | Activate | None             |
|---------+-------------------+------------------+----------+------------------|") (custom-autoload 'hsys-org-enable-smart-keys "hsys-org" t) (defvar hsys-org-mode-function #'hsys-org-mode-p "*Boolean function that returns non-nil when point is in an Org-related buffer.") (autoload 'hsys-org-mode-p "hsys-org" "Return non-nil if point is within an Org major or minor-mode buffer.") (autoload 'hsys-org-at-read-only-p "hsys-org" "Return non-nil if point is in an Org read-only context.") (register-definition-prefixes "hsys-org" '("hsys-org-" "org-")) (autoload 'hsys-org-roam-consult-grep "hsys-org-roam" "Search with the consult org-roam grep command.
Interactively show all matches from `hyrolo-file-list'.
Prompt for the search pattern." t) (autoload 'www-url-expand-file-name "hsys-www" "Expand and return non-url and non-remote PATH in DIR.
Return http urls unchanged.  Normalize remote paths.

(fn PATH &optional DIR)") (autoload 'www-url-find-file-noselect "hsys-www" "Find PATH without selecting its buffer.  Handle http urls.

(fn PATH &rest ARGS)") (autoload 'eww-browse-url "hsys-www" "Ask the eww browser to load URL.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new buffer tab on the window tab-line.  A non-nil
prefix argument reverses the effect of `browse-url-new-window-flag'.

If `tab-bar-mode' is enabled, then whenever a document would
otherwise be loaded in a new buffer, it is loaded in a new tab
in the tab-bar on an existing frame.  See more options in
`eww-browse-url-new-window-is-tab'.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'.

(fn URL &optional NEW-WINDOW)") (register-definition-prefixes "hsys-www" '("www-url")) (require 'hact) (defact yt-info (video-id) "Display a web page with the metadata information about VIDEO-ID." (hact #'actypes::www-url (format "https://mattw.io/youtube-metadata/?url=https://youtu.be/%s&submit=true" video-id))) (defact yt-play (video-id &optional start-time-string end-time-string) "Play a VIDEO-ID from the point specified by optional START-TIME-STRING.
If not given, START-TIME-STRING is set to \"0s\" representing the beginning
of the video.  START-TIME-STRING is a colon-separated hours:minutes:seconds
string, e.g. 1:2:44 (1 hour, two minutes, 45 seconds), where the hours
and minutes are optional." (hact #'actypes::www-url (hsys-youtube-get-url video-id start-time-string end-time-string))) (defact yt-search (search-term) "Search Youtube for SEARCH-TERM." (interactive "sSearch Youtube for: ") (hyperbole-web-search "Youtube" search-term)) (defact yt-url (video-id &optional start-time-string end-time-string) "Return url to play VIDEO-ID from point specified by optional START-TIME-STRING.
Return nil if START-TIME-STRING is given but is invalid.  If not given,
START-TIME-STRING is set to \"0s\" representing the beginning of the video.

START-TIME-STRING is a colon-separated hours:minutes:seconds string,
e.g. 1:2:44 (1 hour, two minutes, 45 seconds), where the hours and
minutes are optional." (if end-time-string (progn (setq start-time-string (hsys-youtube-time-in-seconds start-time-string) end-time-string (hsys-youtube-time-in-seconds end-time-string)) (hsys-youtube-end-url video-id start-time-string end-time-string)) (setq start-time-string (hsys-youtube-time-in-hms start-time-string) end-time-string (hsys-youtube-time-in-hms end-time-string)) (hsys-youtube-start-url video-id start-time-string))) (autoload 'hsys-youtube-play:help "hsys-youtube" "Show in the minibuffer the url for an `hsys-youtube-play' action button, HBUT.
Called when the Assist Key is pressed on such a button.

(fn HBUT)") (defalias 'hsys-youtube-info (htype:symbol 'yt-info 'actypes)) (defalias 'hsys-youtube-play (htype:symbol 'yt-play 'actypes)) (defalias 'hsys-youtube-search (htype:symbol 'yt-search 'actypes)) (autoload 'hsys-youtube-search:help "hsys-youtube" "Display in the minibuffer the Youtube url to search for SEARCH-TERM.

(fn SEARCH-TERM)" t) (defalias 'hsys-youtube-get-url (htype:symbol 'yt-url 'actypes)) (autoload 'hsys-youtube-get-url:help "hsys-youtube" "Show in minibuffer the url from an `hsys-youtube-get-url' action button, HBUT.
Called when the Assist Key is pressed on such a button.

(fn HBUT)") (autoload 'hsys-youtube-end-url "hsys-youtube" "Return url to play VIDEO-ID from optional START-TIME-STRING to END-TIME-STRING.
VIDEO-ID must be a string and can be a video identifier,
e.g. WkwZHSbHmPg, or a full url to the video.

(fn VIDEO-ID &optional START-TIME-STRING END-TIME-STRING)") (autoload 'hsys-youtube-start-url "hsys-youtube" "Return url to play VIDEO-ID starting at beginning or optional START-TIME-STRING.
VIDEO-ID must be a string and can be a video identifier,
e.g. WkwZHSbHmPg, or a full url to the video.

(fn VIDEO-ID &optional START-TIME-STRING)") (autoload 'hsys-youtube-time-in-hms "hsys-youtube" "Return the start time for a Youtube url from START-TIME-STRING.
Start time is returned as hours, minutes and seconds.
Hours and minutes are optional within the START-TIME-STRING, e.g. 1:2:44 (1
hour, two minutes, 45 seconds into a video).  If the START-TIME-STRING
format is invalid, return it unchanged.

(fn START-TIME-STRING)") (autoload 'hsys-youtube-time-in-seconds "hsys-youtube" "Return the number of seconds time for a Youtube url given a START-TIME-STRING.
Hours and minutes are optional within the START-TIME-STRING,
e.g. 1:2:44 (1 hour, two minutes, 45 seconds into a video).  The
formats 1h2m44s or 1h:2m:44s may also be used.  If the
START-TIME-STRING format is invalid, return it unchanged.

(fn START-TIME-STRING)") (register-definition-prefixes "hsys-youtube" '("hsys-youtube-")) (register-definition-prefixes "htz" '("htz:")) (autoload 'hui-copy-to-register "hui" "Copy region or thing into REGISTER.  With prefix arg, delete as well.
Called from program, takes five args: REGISTER, START, END, DELETE-FLAG,
and REGION-FLAG.  START and END are buffer positions indicating what to copy.
The optional argument REGION-FLAG if non-nil, indicates that we're not just
copying some text between START and END, but we're copying the region.

Interactively, reads the register using `register-read-with-preview'.

If called interactively, `transient-mark-mode' is non-nil, and
there is no active region, copy any delimited selectable thing at
point; see `hui:delimited-selectable-thing'.

(fn REGISTER START END &optional DELETE-FLAG REGION-FLAG)" t) (autoload 'hui-kill-ring-save "hui" "Save the active region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

If called interactively, `transient-mark-mode' is non-nil, and
there is no active region, copy any delimited selectable thing at
point; see `hui:delimited-selectable-thing'.

If you want to append the killed region to the last killed text,
use \\[append-next-kill] before \\[kill-ring-save].

The copied text is filtered by `filter-buffer-substring' before it is
saved in the kill ring, so the actual saved text might be different
from what was in the buffer.

When called from Lisp, save in the kill ring the stretch of text
between BEG and END, unless the optional argument REGION is
non-nil, in which case ignore BEG and END, and save the current
region instead.

This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied.

(fn BEG END &optional REGION)" t) (register-definition-prefixes "hui" '("hui:")) (autoload 'smart-dired-sidebar "hui-dired-sidebar" "Use a single key or mouse key to manipulate directory entries.

Invoked via a key press when in dired-sidebar-mode.  It assumes
that its caller has already checked that the key was pressed in
an appropriate buffer and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the item is displayed for editing,
     normally in another window, or if it is a directory and
     `dired-sidebar-cycle-subtree-on-click' is t it will expand
     and collapse the entry
 (2) at the end of an entry line: invoke `action-key-eol-function',
     typically to scroll up proportionally, if an Action Key press; invoke
     `assist-key-eol-function', typically to scroll down proportionally,
     if an Asisst Key press;
 (3) on the first line of the buffer (other than the end of line),
     dired is run on the current directory of this dired-sidebar;
 (4) at the end of the first or last line of the buffer,
     this dired-sidebar invocation is hidden." t) (register-definition-prefixes "hui-em-but" '("hproperty:")) (autoload 'hui-menu-of-buffers "hui-jmenu") (autoload 'hui-menu-screen-commands "hui-jmenu" "Popup a menu of buffers, frames, and windows, allowing user to jump to one." t) (autoload 'hui-menu-jump-to-buffer "hui-jmenu" "Popup a menu of existing buffers categorized by mode name.
Jump to chosen buffer." t) (autoload 'hui-menu-jump-to-frame "hui-jmenu" "Popup a menu of existing frames.  Jump to chosen frame." t) (autoload 'hui-menu-jump-to-window "hui-jmenu" "Popup a menu of existing frames.  Jump to chosen frame." t) (register-definition-prefixes "hui-jmenu" '("hui-menu-")) (register-definition-prefixes "hui-menu" '("hui-menu-" "hyperbole-" "infodock-hyperbole-menu")) (autoload 'hyperbole "hui-mini" "Invoke the Hyperbole minibuffer menu when not already active.
\\[hyperbole] runs this.  Non-interactively, return t if a menu is
displayed by this call, else nil (e.g. when already in a Hyperbole
mini-menu).

Two optional arguments may be given to invoke alternative menus.
MENU (a symbol) specifies the menu to invoke from MENU-LIST, (a
Hyperbole menu list structure).  MENU defaults to \\='hyperbole and MENU-LIST
to `hui:menus'.  See `hui:menus' definition for the format of the menu list
structure.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than display of a menu.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text.

(fn &optional MENU MENU-LIST DOC-FLAG HELP-STRING-FLAG)" t) (autoload 'hyperbole-demo "hui-mini" "Display the Hyperbole FAST-DEMO.
With a prefix arg, display the older, more extensive DEMO file.

(fn &optional ARG)" t) (autoload 'hyperbole-set-key "hui-mini" "In KEYMAP, bind KEY to Hyperbole minibuffer BINDING.
If KEYMAP is nil, use the value of (global-key-map).

KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

BINDING is one of:
  nil     - immediately remove key binding from keymap
  string  - upon key press, execute the BINDING string as a key series
  command - upon key press, run the command interactively.

Note that other local or minor mode bindings may shadow/override any
binding made with this function.

(fn KEYMAP KEY BINDING)" t) (register-definition-prefixes "hui-mini" '("hui" "hyperbole-minibuffer-menu")) (autoload 'smart-eobp "hui-mouse" "Return t if point is past the last visible buffer line with text.") (register-definition-prefixes "hui-mouse" '("action-key-" "assist-key-" "first-line-p" "hkey-" "hmouse-" "last-line-p" "smart-")) (autoload 'hui-register-struct-at-point "hui-register" "Make a Hyperbole link to button register struct for button at point.") (autoload 'hui-select-at-p "hui-select" "Non-nil means character matches a syntax entry in `hui-select-syntax-alist'.
The character is after optional POS or point.  The non-nil value
returned is the function to call to select that syntactic unit.

(fn &optional POS)" t) (autoload 'hui-select-goto-matching-delimiter "hui-select" "Jump back and forth between the start and end delimiters of a thing." t) (autoload 'hui-select-initialize "hui-select" "Initialize the hui-select mode on a double click of the left mouse key.
Also, add language-specific syntax setups to aid in thing selection." t) (autoload 'hui-select-get-thing "hui-select" "Return the thing at point that `hui-select-thing' would select.") (autoload 'hui-select-thing "hui-select" "Select a region based on the syntax of the thing at point.
If invoked repeatedly, this selects bigger and bigger things.
If `hui-select-display-type' is non-nil and this is called
interactively, the type of selection is displayed in the minibuffer." t) (autoload 'hui-select-thing-with-mouse "hui-select" "Select a region based on the syntax of the character from a mouse click EVENT.
If the click occurs at the same point as the last click, select
the next larger syntactic structure.  If `hui-select-display-type' is
non-nil and this is called interactively, the type of selection is
displayed in the minibuffer.

(fn EVENT)" t) (autoload 'hui-select-goto-matching-tag "hui-select" "Move point to start of the tag paired with closest tag point is at or precedes.
Enabled in major modes in `hui-select-markup-modes'.  Returns t if
point is moved, else nil.  Signals an error if no tag is found
following point or if the closing tag does not have a `>'
terminator character." t) (autoload 'hui-select-and-copy-thing "hui-select" "Copy the region surrounding the syntactical unit at point to the kill ring." t) (autoload 'hui-select-and-kill-thing "hui-select" "Kill the region surrounding the syntactical unit at point." t) (autoload 'hui-select-double-click-hook "hui-select" "Select region based on the character syntax where the mouse is double-clicked.
If the double-click occurs at the same point as the last double-click, select
the next larger syntactic structure.  If `hui-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer.

(fn EVENT CLICK-COUNT)") (register-definition-prefixes "hui-select" '("hui-select-")) (autoload 'smart-treemacs "hui-treemacs" "Use a single key or mouse key to manipulate directory entries.

Invoked via a key press when in treemacs-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on an entry icon, the treemacs TAB command is run to expand and
     collapse the entry;
 (2) elsewhere within an entry line, the item is displayed for editing,
     normally in another window;
 (3) at the end of an entry line: invoke `action-key-eol-function',
     typically to scroll up proportionally, if an Action Key press; invoke
     `assist-key-eol-function', typically to scroll down proportionally,
     if an Asisst Key press;
 (4) on the first line of the buffer (other than the end of line),
     dired is run on the current directory of this Treemacs;
 (5) at the end of the first or last line of the buffer,
     this Treemacs invocation is quit." t) (autoload 'smart-treemacs-modeline "hui-treemacs" "Toggle display of Treemacs from Smart Action Key click on a modeline.

When pressed on the Treemacs buffer modeline or Treemacs is displaying
the default directory of the buffer modeline clicked upon, then
quit/hide the Treemacs window.  Otherwise, display the Treemacs window
with the default directory of the buffer modeline clicked upon.

Suitable for use as a value of `action-key-modeline-buffer-id-function'.") (register-definition-prefixes "hui-window" '("action-key-m" "assist-key-m" "hmouse-" "smart-")) (autoload 'var:add-and-run-hook "hvar" "Add to mode HOOK the HOOK-FUNCTION; call it in matching major-mode buffers.
HOOK is a symbol whose name begins with a major-mode name and ends with
\"-hook\".

(fn HOOK HOOK-FUNCTION)") (autoload 'var:append "hvar" "Append to value held by VAR-SYMBOL, LIST-TO-ADD.  Return new value.
If VAR-SYMBOL is unbound, it is set to LIST-TO-ADD.
Use to append to hook variables.  Store all values for later removal.
Do nothing when `inhibit-hyperbole-messaging' is non-nil.

(fn VAR-SYMBOL LIST-TO-ADD)") (register-definition-prefixes "hvar" '("var:")) (register-definition-prefixes "hversion" '("hyperb:" "id-")) (autoload 'Vm-init "hvm" "Initialize Hyperbole support for Vm mail reading." t) (register-definition-prefixes "hvm" '("Vm-" "vm-")) (eval-after-load "buff-menu" '(define-key Buffer-menu-mode-map "@" 'hycontrol-windows-grid)) (eval-after-load "ibuffer" '(define-key ibuffer-mode-map "@" 'hycontrol-windows-grid)) (eval-after-load "dired" '(define-key dired-mode-map "@" 'hycontrol-windows-grid)) (autoload 'hycontrol-enable-frames-mode "hycontrol" "Globally enable HyControl Frames mode for rapid Emacs frame control.

  Interactively delete, jump to, move, replicate, and resize frames.
With optional numeric prefix ARG, move and resize by ARG (an
integer) units.  If ARG is < 1, it is set to 1.  If it is >
`hycontrol-maximum-units', it is set to `hycontrol-maximum-units'.

(fn &optional ARG)" t) (autoload 'hycontrol-enable-windows-mode "hycontrol" "Globally enable HyControl Windows mode for rapid Emacs window control.

Interactively delete, jump to, rebalance, resize, and split windows.
Optional non-negative numeric prefix ARG is used as the number of
units for commands issued while the mode is active.  If ARG is < 1, it
is set to 1.  If it is > `hycontrol-maximum-units', it is set to
`hycontrol-maximum-units'.

(fn &optional ARG)" t) (autoload 'hycontrol-local-frames-mode "hycontrol" "Toggle Hyperbole Frames control minor mode in the current buffer.

This is a minor mode.  If called interactively, toggle the
`Hycontrol-Local-Frames mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `hycontrol-local-frames-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'hycontrol-frames-mode 'globalized-minor-mode t) (defvar hycontrol-frames-mode nil "Non-nil if Hycontrol-Frames mode is enabled.
See the `hycontrol-frames-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hycontrol-frames-mode'.") (custom-autoload 'hycontrol-frames-mode "hycontrol" nil) (autoload 'hycontrol-frames-mode "hycontrol" "Toggle Hycontrol-Local-Frames mode in all buffers.
With prefix ARG, enable Hycontrol-Frames mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Hycontrol-Local-Frames mode is enabled in all buffers where `(lambda
nil (hycontrol-local-frames-mode 1))' would do it.

See `hycontrol-local-frames-mode' for more information on
Hycontrol-Local-Frames mode.

(fn &optional ARG)" t) (autoload 'hycontrol-local-windows-mode "hycontrol" "Toggle Hyperbole Windows control minor mode in the current buffer.

This is a minor mode.  If called interactively, toggle the
`Hycontrol-Local-Windows mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `hycontrol-local-windows-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'hycontrol-windows-mode 'globalized-minor-mode t) (defvar hycontrol-windows-mode nil "Non-nil if Hycontrol-Windows mode is enabled.
See the `hycontrol-windows-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hycontrol-windows-mode'.") (custom-autoload 'hycontrol-windows-mode "hycontrol" nil) (autoload 'hycontrol-windows-mode "hycontrol" "Toggle Hycontrol-Local-Windows mode in all buffers.
With prefix ARG, enable Hycontrol-Windows mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Hycontrol-Local-Windows mode is enabled in all buffers where `(lambda
nil (hycontrol-local-windows-mode 1))' would do it.

See `hycontrol-local-windows-mode' for more information on
Hycontrol-Local-Windows mode.

(fn &optional ARG)" t) (autoload 'hycontrol-frame-adjust-widths "hycontrol" "Cycle through different common width adjustments of a frame.
Widths are given in screen percentages by the list
`hycontrol-frame-widths' and typically go from widest to narrowest." t) (autoload 'hycontrol-frame-adjust-widths-full-height "hycontrol" "Cycle through frame width adjustments after fixing its height full-screen.
Widths are given in screen percentages by the list
`hycontrol-frame-widths' and typically go from widest to narrowest." t) (autoload 'hycontrol-frame-adjust-heights "hycontrol" "Cycle through different common height adjustments of a frame.
Heights are given in screen percentages by the list
`hycontrol-frame-heights' and typically go from tallest to shortest." t) (autoload 'hycontrol-frame-adjust-heights-full-width "hycontrol" "Cycle through height adjustments of a frame after fixing its width full-screen.
Heights are given in screen percentages by the list
`hycontrol-frame-heights' and typically go from tallest to shortest." t) (autoload 'hycontrol-windows-grid "hycontrol" "Display a grid of windows in the selected frame, sized according to prefix ARG.
Left digit of ARG is the number of grid rows and the right digit is
the number of grid columns.  Use {C-h h h} to restore the prior frame
configuration after a grid is displayed.

If ARG is 0, prompt for a major mode whose buffers should be
displayed in the grid windows, then prompt for the grid size.

If ARG is < 0, prompt for a glob-type file pattern and display
files that match the pattern in an abs(ARG) sized windows grid
or an autosized one if the ARG value is invalid.

Otherwise, prompt for the grid size if ARG is an invalid size
(positive and more than two digits).

With a current buffer in Dired, Buffer Menu or IBuffer mode that
contains marked items, the buffers associated with those items
are displayed in the grid (unless ARG is 0).

By default, the most recently used buffers are displayed in each window,
first selecting only those buffers which match any of the
predicate expressions in `hycontrol-display-buffer-predicate-list'.
(The default predicate list chooses buffers with attached files).
Then, if there are not enough buffers for all windows, the buffers
that failed to match to any predicate are used.  In all cases, buffers
whose names start with a space are ignored.

When done, this resets the persistent HyControl prefix argument to 1
to prevent following commands from using the often large grid size
argument.

(fn ARG)" t) (autoload 'hycontrol-windows-grid-by-file-pattern "hycontrol" "Display up to an abs(prefix ARG)-sized window grid of files matching PATTERN.
Use absolute file paths if called interactively or optional
FULL-FLAG is non-nil.  PATTERN is a shell glob pattern.

Left digit of ARG is the number of grid rows and the right digit
is the number of grid columns.  If ARG is nil, 0, 1, less than
11, greater than 99, then autosize the grid to fit the number of
files matched by PATTERN.  Otherwise, if ARG ends in a 0, adjust the
grid size to the closest valid size.

(fn ARG PATTERN &optional FULL-FLAG)" t) (autoload 'hycontrol-windows-grid-by-major-mode "hycontrol" "Display a prefix ARG-sized grid of windows with buffers of major MODE.
Left digit of ARG is the number of grid rows and the right digit
is the number of grid columns.

See documentation of `hycontrol-windows-grid' for further details.

(fn ARG MODE)" t) (autoload 'hycontrol-windows-grid-repeatedly "hycontrol" "Repeatedly display different window grid layouts according to prefix ARG.
ARG is prompted for each time.  Left digit of ARG is the number
of grid rows and the right digit is the number of grid columns.

See documentation of `hycontrol-windows-grid' for further details.

(fn &optional ARG)" t) (autoload 'hycontrol-window-to-new-frame "hycontrol" "Create a new frame sized to match the selected window with the same buffer.
If there is only one window in the source frame or if
`hycontrol-keep-window-flag' is non-nil, leave the original
window and just clone it into the new frame; otherwise, delete
the original window." t) (autoload 'hycontrol-clone-window-to-new-frame "hycontrol" "Create a new frame sized to match the selected window with the same buffer." t) (register-definition-prefixes "hycontrol" '("hycontrol-")) (autoload 'hypb:activate-interaction-log-mode "hypb" "Configure and enable the interaction-log package for use with Hyperbole.
This displays a clean log of Emacs keys used and commands executed." t) (autoload 'hypb:count-visible-windows "hypb" "Return the number of visible, non-minibuffer windows across all frames.") (autoload 'hypb:configuration "hypb" "Insert Emacs configuration information at the end of a buffer.
Use optional OUT-BUF if present, else the current buffer.

(fn &optional OUT-BUF)") (autoload 'hypb:def-to-buffer "hypb" "Copy next optional ARG code definitions to the start of optional BUFFER.
Default ARG is 1 and default BUFFER is \"*scratch*\".  Leave
point at the start of the inserted text.

(fn &optional ARG BUFFER)" t) (autoload 'hypb:devdocs-lookup "hypb" "Prompt for and display a devdocs.io docset section within Emacs.
This will install the Emacs devdocs package if not yet installed." t) (autoload 'hypb:helm-apropos "hypb" "Prompt for and display the doc for a command, function, variable or face.
With optional SYMBOL-NAME non-nil, display the doc for that.
This will this install the Emacs helm package when needed.

(fn &optional SYMBOL-NAME)" t) (autoload 'hypb:helm-info "hypb" "Prompt across all Info manuals and display the node selected.
With optional prefix arg REFRESH non-nil, refresh the cache of Info manuals.
This will this install the Emacs helm package when needed.

(fn &optional REFRESH)" t) (autoload 'hypb:locate "hypb" "Find file name match anywhere and put results in the `*Locate*' buffer.
Pass it SEARCH-STRING as argument.  Interactively, prompt for SEARCH-STRING.
With prefix arg ARG, prompt for the exact shell command to run instead.

This program searches for those file names in a database that match
SEARCH-STRING and normally outputs all matching absolute file names,
one per line.  The database normally consists of all files on your
system, or of all files that you have access to.  Consult the
documentation of the program for the details about how it determines
which file names match SEARCH-STRING.  (Those details vary highly with
the version.)

You can specify another program for this command to run by customizing
the variables `locate-command' or `locate-make-command-line'.

The main use of FILTER is to implement `locate-with-filter'.  See
the docstring of that function for its meaning.

After preparing the results buffer, this runs `dired-mode-hook' and
then `locate-post-command-hook'.

(fn SEARCH-STRING &optional FILTER ARG)" t) (autoload 'hypb:map-plist "hypb" "Apply FUNC of two args, key and value, to key-value pairs in PLIST.

(fn FUNC PLIST)") (autoload 'hypb:require-package "hypb" "Prompt user to install, if necessary, and require the Emacs PACKAGE-NAME.
PACKAGE-NAME may be a symbol or a string.

(fn PACKAGE-NAME)") (autoload 'hypb:rgrep "hypb" "Recursively grep with symbol at point or PATTERN.
Grep over all non-backup and non-autosave files in the current
directory tree.  If in an Emacs Lisp mode buffer and no optional
PREFIX-ARG is given, limit search to only .el and .el.gz files.

(fn PATTERN &optional PREFX-ARG)" t) (autoload 'hypb:display-file-with-logo "hypb" "Display a text FILE in view mode with the Hyperbole banner prepended.
If FILE is not an absolute path, expand it relative to `hyperb:dir'.

(fn FILE)") (register-definition-prefixes "hypb" '("hypb:")) (register-definition-prefixes "hypb-ert" '("hyp")) (register-definition-prefixes "hypb-maintenance" '("hypb:")) (defvar hyperbole-mode nil "Non-nil if Hyperbole mode is enabled.
See the `hyperbole-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hyperbole-mode'.") (custom-autoload 'hyperbole-mode "hyperbole" nil) (autoload 'hyperbole-mode "hyperbole" "Toggle Hyperbole global minor mode.

Hyperbole is the Everyday Hypertextual Information Manager.

When Hyperbole mode is enabled, the `hyperbole-mode' variable
is non-nil, Hyperbole menus are enabled, as are Hyperbole keys.

Invoke the Hyperbole minibuffer menu with \\[hyperbole].  See the
Info documentation at \"(hyperbole)Top\".

\\{hyperbole-mode-map}

This is a global minor mode.  If called interactively, toggle the
`Hyperbole mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='hyperbole-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (let ((us (if (fboundp 'macroexp-file-name) (macroexp-file-name) load-file-name))) (when us (add-to-list 'load-path (expand-file-name "kotl" (file-name-directory us))) (require 'kotl-autoloads nil t))) (register-definition-prefixes "hyperbole" '("hkey-" "hyperb")) (autoload 'hyrolo-initialize-file-list "hyrolo" "Initialize the list of files used for HyRolo search if not already initialized.

(fn &optional FORCE-INIT-FLAG)" t) (make-obsolete 'hyrolo-initialize-file-list 'nil "8.0.1") (autoload 'hyrolo-add "hyrolo" "Add a new entry in personal rolo for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string.

(fn NAME &optional FILE)" t) (autoload 'hyrolo-display-matches "hyrolo" "Display optional DISPLAY-BUF buffer of previously found rolo matches.
If DISPLAY-BUF is nil, use the value in `hyrolo-display-buffer'.
Second arg RETURN-TO-BUFFER is the buffer to leave point within
after the display.

(fn &optional DISPLAY-BUF RETURN-TO-BUFFER)" t) (autoload 'hyrolo-edit "hyrolo" "Edit a rolo entry given by optional NAME within `hyrolo-file-list'.
With prefix argument, prompt for optional FILE within which to
locate entry.  With no NAME arg, simply displays FILE or first
entry in `hyrolo-file-list' in an editable mode.  NAME may be of
the form: parent/child to edit child below a parent entry which
begins with the parent string.

(fn &optional NAME FILE)" t) (autoload 'hyrolo-fgrep "hyrolo" "Display rolo entries matching STRING or a logical match expression.
Return count of matches.

To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or `hyrolo-file-list'.  Default is to find all matching entries.
Each entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil skips retrieval of matching entries.  Optional HEADLINE-ONLY searches
only the first line of entries, not the full text.  Optional NO-DISPLAY non-nil
retrieves entries but does not display them.

Nil value of MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to the inverse of
that number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
`hyrolo-file-list' and the function `hyrolo-fgrep-logical' for documentation on
the logical sexpression matching.

(fn STRING &optional MAX-MATCHES HYROLO-FILE COUNT-ONLY HEADLINE-ONLY NO-DISPLAY)" t) (autoload 'hyrolo-find-file "hyrolo" "Find an optional FILE in `hyrolo-file-list' with FIND-FUNCTION.
Default to the first listed file when not given a prefix arg.
FIND-FUNCTION must return the buffer of the file found but need not
select it.

(fn &optional FILE FIND-FUNCTION &rest ARGS)" t) (autoload 'hyrolo-find-file-noselect "hyrolo" "HyRolo function to read a FILE in literally.
It uses the setting of `hyrolo-find-file-noselect-function'.

(fn &optional FILE)") (autoload 'hyrolo-grep "hyrolo" "Display rolo entries matching REGEXP and return count of matches.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from
optional HYROLO-FILE-OR-BUFS or hyrolo-file-list.  Default is to
find all matching entries.  Each entry is displayed with all of
its sub-entries.  Optional COUNT-ONLY non-nil means don't
retrieve and don't display matching entries.  Optional
HEADLINE-ONLY searches only the first line of entries, not the
full text.  Optional NO-DISPLAY non-nil retrieves entries but
does not display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
`hyrolo-file-list'.

(fn REGEXP &optional MAX-MATCHES HYROLO-FILE-OR-BUFS COUNT-ONLY HEADLINE-ONLY NO-DISPLAY)" t) (autoload 'hyrolo-grep-or-fgrep "hyrolo" "Grep over `hyrolo-file-list' and display the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

(fn &optional ARG)" t) (autoload 'hyrolo-kill "hyrolo" "Kill a rolo entry given by NAME within `hyrolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Return t if entry is killed, nil otherwise.

(fn NAME &optional FILE)" t) (autoload 'hyrolo-sort "hyrolo" "Sort up to 14 levels of entries in HYROLO-FILE (default is personal rolo).
Assume entries are delimited by one or more `*' characters.
Return list of number of groupings at each entry level.

(fn &optional HYROLO-FILE)" t) (autoload 'hyrolo-toggle-datestamps "hyrolo" "Toggle whether datestamps are updated when rolo entries are modified.
With optional ARG, turn them on iff ARG is positive.

(fn &optional ARG)" t) (autoload 'hyrolo-word "hyrolo" "Display rolo entries with whole word match for STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or hyrolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil skips retrieval of matching entries.  Optional HEADLINE-ONLY searches
only the first line of entries, not the full text.  Optional NO-DISPLAY non-nil
retrieves entries but does not display them.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
hyrolo-file-list.

(fn STRING &optional MAX-MATCHES HYROLO-FILE COUNT-ONLY HEADLINE-ONLY NO-DISPLAY)" t) (autoload 'hyrolo-yank "hyrolo" "Insert at point the first rolo entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string.

(fn NAME &optional REGEXP-P)" t) (autoload 'hyrolo-bbdb-fgrep "hyrolo" "Fgrep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match.

(fn &optional ARG)" t) (autoload 'hyrolo-bbdb-grep "hyrolo" "Grep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> \".bbdb\"
======================================================================
* Jones     Tom                <tj@groovycat.org>
* Sera      Kate               <uptown@singular.net>
* Yako      Maso               <ym@destination.ny>

(fn &optional ARG)" t) (autoload 'hyrolo-google-contacts-fgrep "hyrolo" "Fgrep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match.

(fn &optional ARG)" t) (autoload 'hyrolo-google-contacts-grep "hyrolo" "Grep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> <buffer *Google Contacts*>
======================================================================
* Jones     Tom
* Sera      Kate
* Yako      Maso

(fn &optional ARG)" t) (autoload 'hyrolo-helm-org-rifle "hyrolo" "Search with helm and interactively show all matches from `hyrolo-file-list'.
Prompt for the search pattern.
Search only readable .org and .otl files.  With optional prefix
arg CONTEXT-ONLY-FLAG, show one extra line only of context around
a matching line, rather than entire entries.

(fn &optional CONTEXT-ONLY-FLAG)" t) (autoload 'hyrolo-helm-org-rifle-directory "hyrolo" "Interactively search over `org-directory'.
With optional prefix arg CONTEXT-ONLY-FLAG, show one extra line
only of context around a matching line, rather than entire
entries.

(fn &optional CONTEXT-ONLY-FLAG)" t) (autoload 'hyrolo-helm-org-rifle-directories "hyrolo" "Interactively search over Emacs outline format files in rest of DIRS.
Only readable .org and .otl files are searched.  With optional
prefix arg CONTEXT-ONLY-FLAG, show one extra line only of context
around a matching line, rather than entire entries.

(fn &optional CONTEXT-ONLY-FLAG &rest DIRS)" t) (autoload 'hyrolo-org "hyrolo" "Search `org-directory' files for STRING or logic-based matches.
OPTIONAL prefix arg, MAX-MATCHES, limits the number of matches
returned to the number given.

(fn STRING &optional MAX-MATCHES)" t) (autoload 'hyrolo-org-roam "hyrolo" "Search Org Roam directory files for STRING or logical sexpression.
OPTIONAL prefix arg, MAX-MATCHES, limits the number of matches
returned to the number given.

(fn STRING &optional MAX-MATCHES)" t) (autoload 'hyrolo-consult-grep "hyrolo" "Interactively search `hyrolo-file-list' with a consult package grep command.
Use ripgrep (rg) if found, otherwise, plain grep.  Interactively
show all matches from `hyrolo-file-list'.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES if given.

(fn &optional REGEXP MAX-MATCHES)" t) (autoload 'hyrolo-fgrep-directories "hyrolo" "String/logical HyRolo search over files matching FILE-REGEXP in rest of DIRS.

(fn FILE-REGEXP &rest DIRS)") (autoload 'hyrolo-grep-directories "hyrolo" "Regexp HyRolo search over files matching FILE-REGEXP in rest of DIRS.

(fn FILE-REGEXP &rest DIRS)") (register-definition-prefixes "hyrolo" '("hyrolo-")) (autoload 'hyrolo-demo-fgrep "hyrolo-demo" "Display rolo entries in \"DEMO-ROLO.otl\" matching STRING or a logical sexp.
Display to a maximum of optional prefix arg MAX-MATCHES.
Each entry is displayed with all of its sub-entries.

Nil value of MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to the inverse of
that number of entries and omit file headers.

Returns number of entries matched.  See also documentation for
the function `hyrolo-demo-fgrep-logical' for documentation on the
logical sexpression matching.

(fn STRING &optional MAX-MATCHES)" t) (autoload 'hyrolo-demo-fgrep-logical "hyrolo-demo" "Display rolo entries in \"DEMO-ROLO.otl\" matching EXPR.
EXPR may contain prefix logical operators.
If optional COUNT-ONLY is non-nil, don't display entries, return
count of matching entries only.  If optional INCLUDE-SUB-ENTRIES
flag is non-nil, SEXP will be applied across all sub-entries at
once.  Default is to apply SEXP to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries
unless INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT
flag is non-nil.

A complex example of EXPR might be:
  (and (or (not time card) (xor (french balloons) spanish)) teacher pet)
which means:
  Match neither `time' nor `card'
    or
  Matches exactly one of `french balloons' or `spanish'
    and
  Matches `teacher' and `pet'.

Either double quotes or parentheses may be used to group multiple words as a
single argument.

(fn EXPR &optional COUNT-ONLY INCLUDE-SUB-ENTRIES NO-SUB-ENTRIES-OUT)" t) (register-definition-prefixes "hyrolo-demo" '("hyrolo-demo-")) (autoload 'hyrolo-fgrep-logical "hyrolo-logic" "Display rolo entries matching EXPR.
EXPR is a string that may contain sexpression logical prefix operators.
If optional COUNT-ONLY is non-nil, don't display entries, return
count of matching entries only.  If optional INCLUDE-SUB-ENTRIES
flag is non-nil, SEXP will be applied across all sub-entries at
once.  Default is to apply SEXP to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries
unless INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT
flag is non-nil.

A complex example of EXPR might be:
  (and (or (not time card) (xor (and french balloons) spanish)) teacher pet)
which means:
  Match neither `time' nor `card'
    or
  Match exactly one of (`french' and `balloons') or (`spanish').
    and
  Match `teacher' and `pet'.

Either double quotes or parentheses may be used to group multiple words as a
single argument.

(fn EXPR &optional COUNT-ONLY INCLUDE-SUB-ENTRIES NO-SUB-ENTRIES-OUT WHOLE-BUFFER-FLAG)" t) (register-definition-prefixes "hyrolo-logic" '("hyrolo-")) (register-definition-prefixes "hyrolo-menu" '("hyrolo-" "id-popup-hyrolo-menu" "infodock-hyrolo-menu")) (autoload 'hywconfig-add-by-name "hywconfig" "Save the current window configuration under the string NAME.
When called interactively and a window configuration already exists under
NAME, confirms whether or not to replace it.

(fn NAME)" t) (autoload 'hywconfig-delete-by-name "hywconfig" "Deletes frame-specific window configuration saved under NAME.

(fn NAME)" t) (autoload 'hywconfig-restore-by-name "hywconfig" "Restore frame-specific window configuration saved under NAME.

(fn NAME)" t) (autoload 'hywconfig-delete-pop "hywconfig" "Replace the current frame's window configuration with the most recently saved.
Then deletes this new configuration from the ring." t) (autoload 'hywconfig-delete-pop-continue "hywconfig" "Replace current frame's window configuration with the most recently saved.
Delete this new configuration from the ring.  If the hywconfig
ring is not empty, then stay in the hywconfig menu." t) (autoload 'hywconfig-ring-empty-p "hywconfig" "Return t if the wconfig ring for the current frame is empty; nil otherwise.") (autoload 'hywconfig-ring-save "hywconfig" "Save the current frame's window configuration onto the save ring.
Use {\\[hywconfig-yank-pop]} to restore it at a later time." t) (autoload 'hywconfig-yank-pop "hywconfig" "Replace current frame's window config with prefix arg Nth prior one in ring.
Interactively, default value of N = 1, means the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the
oldest one comes the newest one.

(fn N)" t) (autoload 'hywconfig-yank-pop-continue "hywconfig" "Replace current frame's window config with prefix arg Nth prior one in ring.
If there are more than one entries in the ring, then stay in the hywconfig menu.

Interactively, default value of N = 1, means the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the
oldest one comes the newest one.

(fn N)" t) (register-definition-prefixes "hywconfig" '("hywconfig-")) (autoload 'set:create "set" "Return a new set created from any number of ELEMENTS.
If no ELEMENTS are given, return the empty set.  Uses `set:equal-op'
for comparison.

(fn &rest ELEMENTS)") (register-definition-prefixes "set" '("set:")) (provide 'hyperbole-autoloads)) "vertico-posframe" ((vertico-posframe vertico-posframe-autoloads) (defvar vertico-posframe-mode nil "Non-nil if Vertico-Posframe mode is enabled.
See the `vertico-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-posframe-mode'.") (custom-autoload 'vertico-posframe-mode "vertico-posframe" nil) (autoload 'vertico-posframe-mode "vertico-posframe" "Display Vertico in posframe instead of the minibuffer.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'vertico-posframe-cleanup "vertico-posframe" "Remove frames and buffers used for vertico-posframe." t) (register-definition-prefixes "vertico-posframe" '("vertico-posframe-")) (provide 'vertico-posframe-autoloads)) "org-appear" ((org-appear org-appear-autoloads) (autoload 'org-appear-mode "org-appear" "A minor mode that automatically toggles elements in Org mode.

This is a minor mode.  If called interactively, toggle the
`Org-Appear mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-appear-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "org-appear" '("org-appear-")) (provide 'org-appear-autoloads)) "eshell-git-prompt" ((eshell-git-prompt eshell-git-prompt-autoloads) (autoload 'eshell-git-prompt-use-theme "eshell-git-prompt" "Pick up a Eshell prompt theme from `eshell-git-prompt-themes' to use.

(fn THEME)" t) (autoload 'eshell/use-theme "eshell-git-prompt" "List all available themes and pick one from Eshell.

(fn &optional THEME)") (register-definition-prefixes "eshell-git-prompt" '("eshell-git-prompt-" "with-face")) (provide 'eshell-git-prompt-autoloads)) "eshell-syntax-highlighting" ((eshell-syntax-highlighting eshell-syntax-highlighting-autoloads) (autoload 'eshell-syntax-highlighting-mode "eshell-syntax-highlighting" "Toggle syntax highlighting for Eshell.

This is a minor mode.  If called interactively, toggle the
`Eshell-Syntax-Highlighting mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `eshell-syntax-highlighting-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'eshell-syntax-highlighting-global-mode 'globalized-minor-mode t) (defvar eshell-syntax-highlighting-global-mode nil "Non-nil if Eshell-Syntax-Highlighting-Global mode is enabled.
See the `eshell-syntax-highlighting-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eshell-syntax-highlighting-global-mode'.") (custom-autoload 'eshell-syntax-highlighting-global-mode "eshell-syntax-highlighting" nil) (autoload 'eshell-syntax-highlighting-global-mode "eshell-syntax-highlighting" "Toggle Eshell-Syntax-Highlighting mode in all buffers.
With prefix ARG, enable Eshell-Syntax-Highlighting-Global mode if ARG
is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Eshell-Syntax-Highlighting mode is enabled in all buffers where
`eshell-syntax-highlighting--global-on' would do it.

See `eshell-syntax-highlighting-mode' for more information on
Eshell-Syntax-Highlighting mode.

(fn &optional ARG)" t) (register-definition-prefixes "eshell-syntax-highlighting" '("eshell-syntax-highlighting-")) (provide 'eshell-syntax-highlighting-autoloads)) "company-shell" ((company-shell-autoloads company-shell) (autoload 'company-shell-rebuild-cache "company-shell" "Builds the cache of all completions found on the $PATH and all fish functions." t) (autoload 'company-fish-shell "company-shell" "Company backend for fish shell functions.

(fn COMMAND &optional ARG &rest IGNORED)" t) (autoload 'company-shell "company-shell" "Company mode backend for binaries found on the $PATH.

(fn COMMAND &optional ARG &rest IGNORED)" t) (autoload 'company-shell-env "company-shell" "Company backend for environment variables.

(fn COMMAND &optional ARG &rest IGNORED)" t) (register-definition-prefixes "company-shell" '("company-")) (provide 'company-shell-autoloads)) "tablist" ((tablist-autoloads tablist tablist-filter) (autoload 'tablist-minor-mode "tablist" "Toggle tablist minor mode.

This is a minor mode.  If called interactively, toggle the
`Tablist minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tablist-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'tablist-mode "tablist" "

(fn)" t) (register-definition-prefixes "tablist" '("tablist-")) (register-definition-prefixes "tablist-filter" '("tablist-filter-")) (provide 'tablist-autoloads)) "pdf-tools" ((pdf-isearch pdf-outline pdf-util pdf-tools-autoloads pdf-misc pdf-occur pdf-sync pdf-history pdf-macs pdf-dev pdf-info pdf-loader pdf-virtual pdf-tools pdf-view pdf-annot pdf-links pdf-cache) (autoload 'pdf-annot-minor-mode "pdf-annot" "Support for PDF Annotations.

\\{pdf-annot-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Annot minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-annot-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-annot" '("pdf-annot-")) (register-definition-prefixes "pdf-cache" '("boundingbox" "define-pdf-cache-function" "page" "pdf-cache-" "textregions")) (register-definition-prefixes "pdf-dev" '("pdf-dev-")) (autoload 'pdf-history-minor-mode "pdf-history" "Keep a history of previously visited pages.

This is a simple stack-based history.  Turning the page or
following a link pushes the left-behind page on the stack, which
may be navigated with the following keys.

\\{pdf-history-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-History minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-history-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-history" '("pdf-history-")) (register-definition-prefixes "pdf-info" '("pdf-info-")) (autoload 'pdf-isearch-minor-mode "pdf-isearch" "Isearch mode for PDF buffer.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
(`pdf-isearch-batch-mode'), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

The kind of highlighting is determined by three faces
`pdf-isearch-match' (for the current match), `pdf-isearch-lazy'
(for all other matches) and `pdf-isearch-batch' (when in batch
mode), which see.

Colors may also be influenced by the minor-mode
`pdf-view-dark-minor-mode'.  If this is minor mode enabled, each face's
dark colors, are used (see e.g. `frame-background-mode'), instead
of the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available. Note
that not every isearch command work as expected.

\\{pdf-isearch-active-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Isearch minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-isearch-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-isearch" '("pdf-isearch-")) (autoload 'pdf-links-minor-mode "pdf-links" "Handle links in PDF documents.\\<pdf-links-minor-mode-map>

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Links minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-links-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-links-action-perform "pdf-links" "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page.

(fn LINK)" t) (register-definition-prefixes "pdf-links" '("pdf-links-")) (autoload 'pdf-loader-install "pdf-loader" "Prepare Emacs for using PDF Tools.

This function acts as a replacement for `pdf-tools-install' and
makes Emacs load and use PDF Tools as soon as a PDF file is
opened, but not sooner.

The arguments are passed verbatim to `pdf-tools-install', which
see.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)") (register-definition-prefixes "pdf-loader" '("pdf-loader--")) (register-definition-prefixes "pdf-macs" '("pdf-view-")) (autoload 'pdf-misc-minor-mode "pdf-misc" "FIXME:  Not documented.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-misc-size-indication-minor-mode "pdf-misc" "Provide a working size indication in the mode-line.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Size-Indication minor mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-size-indication-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-misc-menu-bar-minor-mode "pdf-misc" "Display a PDF Tools menu in the menu-bar.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Menu-Bar minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-menu-bar-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-misc-context-menu-minor-mode "pdf-misc" "Provide a right-click context menu in PDF buffers.

\\{pdf-misc-context-menu-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Context-Menu minor mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-context-menu-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-misc" '("pdf-misc-")) (autoload 'pdf-occur "pdf-occur" "List lines matching STRING or PCRE.

Interactively search for a regexp. Unless a prefix arg was given,
in which case this functions performs a string search.

If `pdf-occur-prefer-string-search' is non-nil, the meaning of
the prefix-arg is inverted.

(fn STRING &optional REGEXP-P)" t) (autoload 'pdf-occur-multi-command "pdf-occur" "Perform `pdf-occur' on multiple buffer.

For a programmatic search of multiple documents see
`pdf-occur-search'." t) (defvar pdf-occur-global-minor-mode nil "Non-nil if Pdf-Occur-Global minor mode is enabled.
See the `pdf-occur-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-occur-global-minor-mode'.") (custom-autoload 'pdf-occur-global-minor-mode "pdf-occur" nil) (autoload 'pdf-occur-global-minor-mode "pdf-occur" "Enable integration of Pdf Occur with other modes.

This global minor mode enables (or disables)
`pdf-occur-ibuffer-minor-mode' and `pdf-occur-dired-minor-mode'
in all current and future ibuffer/dired buffer.

This is a global minor mode.  If called interactively, toggle the
`Pdf-Occur-Global minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pdf-occur-global-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-occur-ibuffer-minor-mode "pdf-occur" "Hack into ibuffer's do-occur binding.

This mode remaps `ibuffer-do-occur' to
`pdf-occur-ibuffer-do-occur', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `ibuffer-do-occur'.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Ibuffer minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-occur-ibuffer-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-occur-dired-minor-mode "pdf-occur" "Hack into dired's `dired-do-search' binding.

This mode remaps `dired-do-search' to
`pdf-occur-dired-do-search', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `dired-do-search'.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Dired minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-occur-dired-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-occur" '("pdf-occur-")) (autoload 'pdf-outline-minor-mode "pdf-outline" "Display an outline of a PDF document.

This provides a PDF's outline on the menu bar via imenu.
Additionally the same outline may be viewed in a designated
buffer.

\\{pdf-outline-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Outline minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-outline-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-outline "pdf-outline" "Display an PDF outline of BUFFER.

BUFFER defaults to the current buffer.  Select the outline
buffer, unless NO-SELECT-WINDOW-P is non-nil.

(fn &optional BUFFER NO-SELECT-WINDOW-P)" t) (autoload 'pdf-outline-imenu-enable "pdf-outline" "Enable imenu in the current PDF buffer." t) (register-definition-prefixes "pdf-outline" '("pdf-outline")) (autoload 'pdf-sync-minor-mode "pdf-sync" "Correlate a PDF position with the TeX file.

\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to `synctex' (before AUCTeX
is loaded) and enabling `TeX-source-correlate-mode'.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will
open the corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' (the default is `C-c C-g')
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX.

This is a minor mode.  If called interactively, toggle the
`Pdf-Sync minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-sync-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-sync" '("pdf-sync-")) (defvar pdf-tools-handle-upgrades t "Whether PDF Tools should handle upgrading itself.") (custom-autoload 'pdf-tools-handle-upgrades "pdf-tools" t) (autoload 'pdf-tools-install "pdf-tools" "Install PDF-Tools in all current and future PDF buffers.

If the `pdf-info-epdfinfo-program' is not running or does not
appear to be working, attempt to rebuild it.  If this build
succeeded, continue with the activation of the package.
Otherwise fail silently, i.e. no error is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Don't attempt to install system packages, if SKIP-DEPENDENCIES-P
is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Attempt to install system packages (even if it is deemed
unnecessary), if FORCE-DEPENDENCIES-P is non-nil.

Note that SKIP-DEPENDENCIES-P and FORCE-DEPENDENCIES-P are
mutually exclusive.

Note further, that you can influence the installation directory
by setting `pdf-info-epdfinfo-program' to an appropriate
value (e.g. ~/bin/epdfinfo) before calling this function.

See `pdf-view-mode' and `pdf-tools-enabled-modes'.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" t) (autoload 'pdf-tools-enable-minor-modes "pdf-tools" "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'.

(fn &optional MODES)" t) (autoload 'pdf-tools-help "pdf-tools" "Show a Help buffer for `pdf-tools'." t) (register-definition-prefixes "pdf-tools" '("pdf-tools-")) (register-definition-prefixes "pdf-util" '("display-buffer-split-below-and-attach" "pdf-util-")) (autoload 'pdf-view-bookmark-jump-handler "pdf-view" "The bookmark handler-function interface for bookmark BMK.

See also `pdf-view-bookmark-make-record'.

(fn BMK)") (register-definition-prefixes "pdf-view" '("cua-copy-region--pdf-view-advice" "pdf-view-")) (autoload 'pdf-virtual-edit-mode "pdf-virtual" "Major mode when editing a virtual PDF buffer.

(fn)" t) (autoload 'pdf-virtual-view-mode "pdf-virtual" "Major mode in virtual PDF buffers.

(fn)" t) (defvar pdf-virtual-global-minor-mode nil "Non-nil if Pdf-Virtual-Global minor mode is enabled.
See the `pdf-virtual-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-virtual-global-minor-mode'.") (custom-autoload 'pdf-virtual-global-minor-mode "pdf-virtual" nil) (autoload 'pdf-virtual-global-minor-mode "pdf-virtual" "Enable recognition and handling of VPDF files.

This is a global minor mode.  If called interactively, toggle the
`Pdf-Virtual-Global minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pdf-virtual-global-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-virtual-buffer-create "pdf-virtual" "

(fn &optional FILENAMES BUFFER-NAME DISPLAY-P)" t) (register-definition-prefixes "pdf-virtual" '("pdf-virtual-")) (provide 'pdf-tools-autoloads)) "kv" ((kv-autoloads kv) (register-definition-prefixes "kv" '("dotass" "keyword->symbol" "map-bind")) (provide 'kv-autoloads)) "esxml" ((esxml esxml-autoloads esxml-pkg esxml-query) (register-definition-prefixes "esxml" '("attr" "esxml-" "pp-esxml-to-xml" "string-trim-whitespace" "sxml-to-" "xml-to-esxml")) (register-definition-prefixes "esxml-query" '("esxml-")) (provide 'esxml-autoloads)) "nov" ((nov-autoloads nov) (autoload 'nov-mode "nov" "Major mode for reading EPUB documents

(fn)" t) (autoload 'nov-bookmark-jump-handler "nov" "The bookmark handler-function interface for bookmark BMK.

See also `nov-bookmark-make-record'.

(fn BMK)") (register-definition-prefixes "nov" '("nov-")) (provide 'nov-autoloads)) "ht" ((ht ht-autoloads) (register-definition-prefixes "ht" 'nil) (provide 'ht-autoloads)) "ox-pandoc" ((ox-pandoc ox-pandoc-autoloads) (autoload 'org-pandoc-export-to-asciidoc "ox-pandoc" "Export to asciidoc.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-asciidoc-and-open "ox-pandoc" "Export to asciidoc and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-asciidoc "ox-pandoc" "Export as asciidoc.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer "ox-pandoc" "Export to beamer.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer-and-open "ox-pandoc" "Export to beamer and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-beamer "ox-pandoc" "Export as beamer.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer-pdf "ox-pandoc" "Export to beamer-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer-pdf-and-open "ox-pandoc" "Export to beamer-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-commonmark "ox-pandoc" "Export to commonmark.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-commonmark-and-open "ox-pandoc" "Export to commonmark and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-commonmark "ox-pandoc" "Export as commonmark.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context "ox-pandoc" "Export to context.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context-and-open "ox-pandoc" "Export to context and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-context "ox-pandoc" "Export as context.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context-pdf "ox-pandoc" "Export to context-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context-pdf-and-open "ox-pandoc" "Export to context-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook4 "ox-pandoc" "Export to docbook4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook4-and-open "ox-pandoc" "Export to docbook4 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-docbook4 "ox-pandoc" "Export as docbook4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook5 "ox-pandoc" "Export to docbook5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook5-and-open "ox-pandoc" "Export to docbook5 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-docbook5 "ox-pandoc" "Export as docbook5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docx "ox-pandoc" "Export to docx.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docx-and-open "ox-pandoc" "Export to docx and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dokuwiki "ox-pandoc" "Export to dokuwiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dokuwiki-and-open "ox-pandoc" "Export to dokuwiki and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-dokuwiki "ox-pandoc" "Export as dokuwiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dzslides "ox-pandoc" "Export to dzslides.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dzslides-and-open "ox-pandoc" "Export to dzslides and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-dzslides "ox-pandoc" "Export as dzslides.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub2 "ox-pandoc" "Export to epub2.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub2-and-open "ox-pandoc" "Export to epub2 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub3 "ox-pandoc" "Export to epub3.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub3-and-open "ox-pandoc" "Export to epub3 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-fb2 "ox-pandoc" "Export to fb2.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-fb2-and-open "ox-pandoc" "Export to fb2 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-fb2 "ox-pandoc" "Export as fb2.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-gfm "ox-pandoc" "Export to gfm.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-gfm-and-open "ox-pandoc" "Export to gfm and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-gfm "ox-pandoc" "Export as gfm.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-haddock "ox-pandoc" "Export to haddock.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-haddock-and-open "ox-pandoc" "Export to haddock and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-haddock "ox-pandoc" "Export as haddock.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html4 "ox-pandoc" "Export to html4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html4-and-open "ox-pandoc" "Export to html4 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-html4 "ox-pandoc" "Export as html4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5 "ox-pandoc" "Export to html5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5-and-open "ox-pandoc" "Export to html5 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-html5 "ox-pandoc" "Export as html5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5-pdf "ox-pandoc" "Export to html5-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5-pdf-and-open "ox-pandoc" "Export to html5-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-icml "ox-pandoc" "Export to icml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-icml-and-open "ox-pandoc" "Export to icml and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-icml "ox-pandoc" "Export as icml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-jats "ox-pandoc" "Export to jats.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-jats-and-open "ox-pandoc" "Export to jats and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-jats "ox-pandoc" "Export as jats.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-jira "ox-pandoc" "Export to jira.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-jira "ox-pandoc" "Export as jira.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-json "ox-pandoc" "Export to json.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-json-and-open "ox-pandoc" "Export to json and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-json "ox-pandoc" "Export as json.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex "ox-pandoc" "Export to latex.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex-and-open "ox-pandoc" "Export to latex and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-latex "ox-pandoc" "Export as latex.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex-pdf "ox-pandoc" "Export to latex-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex-pdf-and-open "ox-pandoc" "Export to latex-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-man "ox-pandoc" "Export to man.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-man-and-open "ox-pandoc" "Export to man and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-man "ox-pandoc" "Export as man.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown "ox-pandoc" "Export to markdown.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown-and-open "ox-pandoc" "Export to markdown and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown "ox-pandoc" "Export as markdown.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_mmd "ox-pandoc" "Export to markdown_mmd.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_mmd-and-open "ox-pandoc" "Export to markdown_mmd and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown_mmd "ox-pandoc" "Export as markdown_mmd.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_phpextra "ox-pandoc" "Export to markdown_phpextra.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_phpextra-and-open "ox-pandoc" "Export to markdown_phpextra and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown_phpextra "ox-pandoc" "Export as markdown_phpextra.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_strict "ox-pandoc" "Export to markdown_strict.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_strict-and-open "ox-pandoc" "Export to markdown_strict and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown_strict "ox-pandoc" "Export as markdown_strict.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-mediawiki "ox-pandoc" "Export to mediawiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-mediawiki-and-open "ox-pandoc" "Export to mediawiki and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-mediawiki "ox-pandoc" "Export as mediawiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms "ox-pandoc" "Export to ms.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms-and-open "ox-pandoc" "Export to ms and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-ms "ox-pandoc" "Export as ms.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms-pdf "ox-pandoc" "Export to ms-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms-pdf-and-open "ox-pandoc" "Export to ms-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-muse "ox-pandoc" "Export to muse.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-muse-and-open "ox-pandoc" "Export to muse and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-muse "ox-pandoc" "Export as muse.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-native "ox-pandoc" "Export to native.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-native-and-open "ox-pandoc" "Export to native and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-native "ox-pandoc" "Export as native.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-odt "ox-pandoc" "Export to odt.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-odt-and-open "ox-pandoc" "Export to odt and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opendocument "ox-pandoc" "Export to opendocument.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opendocument-and-open "ox-pandoc" "Export to opendocument and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-opendocument "ox-pandoc" "Export as opendocument.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opml "ox-pandoc" "Export to opml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opml-and-open "ox-pandoc" "Export to opml and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-opml "ox-pandoc" "Export as opml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-org "ox-pandoc" "Export to org.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-org-and-open "ox-pandoc" "Export to org and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-org "ox-pandoc" "Export as org.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-plain "ox-pandoc" "Export to plain.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-plain-and-open "ox-pandoc" "Export to plain and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-plain "ox-pandoc" "Export as plain.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-pptx "ox-pandoc" "Export to pptx.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-pptx-and-open "ox-pandoc" "Export to pptx and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-revealjs "ox-pandoc" "Export to revealjs.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-revealjs-and-open "ox-pandoc" "Export to revealjs and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-revealjs "ox-pandoc" "Export as revealjs.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rst "ox-pandoc" "Export to rst.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rst-and-open "ox-pandoc" "Export to rst and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-rst "ox-pandoc" "Export as rst.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rtf "ox-pandoc" "Export to rtf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rtf-and-open "ox-pandoc" "Export to rtf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-rtf "ox-pandoc" "Export as rtf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-s5 "ox-pandoc" "Export to s5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-s5-and-open "ox-pandoc" "Export to s5 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-s5 "ox-pandoc" "Export as s5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slideous "ox-pandoc" "Export to slideous.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slideous-and-open "ox-pandoc" "Export to slideous and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-slideous "ox-pandoc" "Export as slideous.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slidy "ox-pandoc" "Export to slidy.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slidy-and-open "ox-pandoc" "Export to slidy and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-slidy "ox-pandoc" "Export as slidy.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-tei "ox-pandoc" "Export to tei.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-tei-and-open "ox-pandoc" "Export to tei and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-tei "ox-pandoc" "Export as tei.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-texinfo "ox-pandoc" "Export to texinfo.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-texinfo-and-open "ox-pandoc" "Export to texinfo and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-texinfo "ox-pandoc" "Export as texinfo.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-textile "ox-pandoc" "Export to textile.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-textile-and-open "ox-pandoc" "Export to textile and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-textile "ox-pandoc" "Export as textile.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-zimwiki "ox-pandoc" "Export to zimwiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-zimwiki-and-open "ox-pandoc" "Export to zimwiki and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-zimwiki "ox-pandoc" "Export as zimwiki.

(fn &optional A S V B E)" t) (register-definition-prefixes "ox-pandoc" '("org-pandoc-")) (provide 'ox-pandoc-autoloads)) "org-view-mode" ((org-view-mode org-view-mode-autoloads org-view-font) (defvar org-view-font-remaps '((default :family "Serif" :height 1.25) (org-document-title :height 1.2) (org-level-1 :height 1.4 :weight normal) (org-level-2 :height 1.3 :weight normal) (org-level-3 :height 1.2 :weight normal) (org-level-4 :height 1.1 :slant italic) (org-level-5 :height 1.0 :weight semibold) (org-level-6 :height 1.0 :weight semibold) (org-level-7 :height 1.0 :weight semibold) (org-level-8 :height 1.0 :weight semibold) (org-link :underline t :weight normal)) "Faces to remap, with attributes to remap.") (custom-autoload 'org-view-font-remaps "org-view-font" t) (defvar org-view-font-no-remap '(org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-latex-and-related org-checkbox org-meta-line org-table org-drawer org-special-keyword org-property-value org-verbatim) "Faces to avoid remapping.") (custom-autoload 'org-view-font-no-remap "org-view-font" t) (defvar org-view-font-enable nil "Enable the custom reading font settings for `org-view-mode'.") (custom-autoload 'org-view-font-enable "org-view-font" t) (autoload 'org-view-font-enable-font "org-view-font" "Enable the org-view-font feature.") (autoload 'org-view-font-disable-font "org-view-font" "Disable the org-view-font feature.") (register-definition-prefixes "org-view-font" '("org-view-font--")) (autoload 'org-view-mode "org-view-mode" "Hide/show babel source code blocks on demand.

This is a minor mode.  If called interactively, toggle the
`org-view mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-view-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "org-view-mode" '("org-view-")) (provide 'org-view-mode-autoloads)) "request" ((request-autoloads request) (autoload 'request-response-header "request" "Fetch the values of RESPONSE header field named FIELD-NAME.

It returns comma separated values when the header has multiple
field with the same name, as :RFC:`2616` specifies.

Examples::

  (request-response-header response
                           \"content-type\") ; => \"text/html; charset=utf-8\"
  (request-response-header response
                           \"unknown-field\") ; => nil

(fn RESPONSE FIELD-NAME)") (autoload 'request-response-headers "request" "Return RESPONSE headers as an alist.
I would have chosen a function name that wasn't so suggestive that
`headers` is a member of the `request-response` struct, but
as there's already precedent with `request-response-header', I
hew to consistency.

(fn RESPONSE)") (autoload 'request "request" "Main entry requesting URL with property list SETTINGS as follow.

==================== ========================================================
Keyword argument      Explanation
==================== ========================================================
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
DATA    (string/alist)   data to be sent to the server
FILES          (alist)   files to be sent to the server (see below)
PARSER        (symbol)   a function that reads current buffer and return data
HEADERS        (alist)   additional headers to send with the request
ENCODING      (symbol)   encoding for request body (utf-8 by default)
SUCCESS     (function)   called on success
ERROR       (function)   called on error
COMPLETE    (function)   called on both success and error
TIMEOUT       (number)   timeout in second
STATUS-CODE    (alist)   map status code (int) to callback
SYNC            (bool)   If non-nil, wait until request is done. Default is nil.
==================== ========================================================


* Callback functions

Callback functions STATUS, ERROR, COMPLETE and `cdr\\='s in element of
the alist STATUS-CODE take same keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it\\='s better to use `&allow-other-keys\\=' [#]_).::

    (CALLBACK                      ; SUCCESS/ERROR/COMPLETE/STATUS-CODE
     :data          data           ; whatever PARSER function returns, or nil
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA), or nil
     :symbol-status symbol-status  ; success/error/timeout/abort/parse-error
     :response      response       ; request-response object
     ...)

.. [#] `&allow-other-keys\\=' is a special \"markers\" available in macros
   in the CL library for function definition such as `cl-defun\\=' and
   `cl-function\\='.  Without this marker, you need to specify all arguments
   to be passed.  This becomes problem when request.el adds new arguments
   when calling callback functions.  If you use `&allow-other-keys\\='
   (or manually ignore other arguments), your code is free from this
   problem.  See info node `(cl) Argument Lists\\=' for more information.

Arguments data, error-thrown, symbol-status can be accessed by
`request-response-data\\=', `request-response-error-thrown\\=',
`request-response-symbol-status\\=' accessors, i.e.::

    (request-response-data RESPONSE)  ; same as data

Response object holds other information which can be accessed by
the following accessors:
`request-response-status-code\\=',
`request-response-url\\=' and
`request-response-settings\\='

* STATUS-CODE callback

STATUS-CODE is an alist of the following format::

    ((N-1 . CALLBACK-1)
     (N-2 . CALLBACK-2)
     ...)

Here, N-1, N-2,... are integer status codes such as 200.


* FILES

FILES is an alist of the following format::

    ((NAME-1 . FILE-1)
     (NAME-2 . FILE-2)
     ...)

where FILE-N is a list of the form::

    (FILENAME &key PATH BUFFER STRING MIME-TYPE)

FILE-N can also be a string (path to the file) or a buffer object.
In that case, FILENAME is set to the file name or buffer name.

Example FILES argument::

    `((\"passwd\"   . \"/etc/passwd\")                ; filename = passwd
      (\"scratch\"  . ,(get-buffer \"*scratch*\"))    ; filename = *scratch*
      (\"passwd2\"  . (\"password.txt\" :file \"/etc/passwd\"))
      (\"scratch2\" . (\"scratch.txt\"  :buffer ,(get-buffer \"*scratch*\")))
      (\"data\"     . (\"data.csv\"     :data \"1,2,3\\n4,5,6\\n\")))

.. note:: FILES is implemented only for curl backend for now.
   As furl.el_ supports multipart POST, it should be possible to
   support FILES in pure elisp by making furl.el_ another backend.
   Contributions are welcome.

   .. _furl.el: https://code.google.com/p/furl-el/


* PARSER function

PARSER function takes no argument and it is executed in the
buffer with HTTP response body.  The current position in the HTTP
response buffer is at the beginning of the buffer.  As the HTTP
header is stripped off, the cursor is actually at the beginning
of the response body.  So, for example, you can pass `json-read\\='
to parse JSON object in the buffer.  To fetch whole response as a
string, pass `buffer-string\\='.

When using `json-read\\=', it is useful to know that the returned
type can be modified by `json-object-type\\=', `json-array-type\\=',
`json-key-type\\=', `json-false\\=' and `json-null\\='.  See docstring of
each function for what it does.  For example, to convert JSON
objects to plist instead of alist, wrap `json-read\\=' by `lambda\\='
like this.::

    (request
     \"https://...\"
     :parser (lambda ()
               (let ((json-object-type \\='plist))
                 (json-read)))
     ...)

This is analogous to the `dataType\\=' argument of jQuery.ajax_.
Only this function can access to the process buffer, which
is killed immediately after the execution of this function.

* SYNC

Synchronous request is functional, but *please* don\\='t use it
other than testing or debugging.  Emacs users have better things
to do rather than waiting for HTTP request.  If you want a better
way to write callback chains, use `request-deferred\\='.

If you can\\='t avoid using it (e.g., you are inside of some hook
which must return some value), make sure to set TIMEOUT to
relatively small value.

Due to limitation of `url-retrieve-synchronously\\=', response slots
`request-response-error-thrown\\=', `request-response-history\\=' and
`request-response-url\\=' are unknown (always nil) when using
synchronous request with `url-retrieve\\=' backend.

* Note

API of `request\\=' is somewhat mixture of jQuery.ajax_ (Javascript)
and requests.request_ (Python).

.. _jQuery.ajax: https://api.jquery.com/jQuery.ajax/
.. _requests.request: https://docs.python-requests.org

(fn URL &rest SETTINGS &key (PARAMS nil) (DATA nil) (HEADERS nil) (ENCODING \\='utf-8) (ERROR nil) (SYNC nil) (RESPONSE (make-request-response)) &allow-other-keys)") (function-put 'request 'lisp-indent-function 'defun) (autoload 'request-untrampify-filename "request" "Return FILE as the local file name.

(fn FILE)") (autoload 'request-abort "request" "Abort request for RESPONSE (the object returned by `request').
Note that this function invoke ERROR and COMPLETE callbacks.
Callbacks may not be called immediately but called later when
associated process is exited.

(fn RESPONSE)") (register-definition-prefixes "request" '("request-")) (provide 'request-autoloads)) "calibredb" ((calibredb-ivy calibredb-transient calibredb-faces calibredb-dired calibredb calibredb-show calibredb-search calibredb-core calibredb-annotation calibredb-org calibredb-opds calibredb-library calibredb-utils calibredb-consult calibredb-helm calibredb-autoloads) (autoload 'calibredb "calibredb" "Enter calibre Search Buffer." t) (register-definition-prefixes "calibredb-annotation" '("calibredb-")) (register-definition-prefixes "calibredb-consult" '("calibredb-consult-read")) (register-definition-prefixes "calibredb-core" '("calibredb-")) (register-definition-prefixes "calibredb-dired" '("calibredb-dired-")) (register-definition-prefixes "calibredb-helm" '("calibredb-")) (autoload 'calibredb-find-counsel "calibredb-ivy" "Use counsel to list all ebooks details." t) (register-definition-prefixes "calibredb-ivy" '("calibredb-")) (autoload 'calibredb-switch-library "calibredb-library" "Swich Calibre Library." t) (autoload 'calibredb-library-list "calibredb-library" "Switch library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item." t) (register-definition-prefixes "calibredb-library" '("calibredb-")) (register-definition-prefixes "calibredb-opds" '("calibredb-opds-")) (autoload 'calibredb-org-link-view "calibredb-org" "Follow calibredb org links by ID.

(fn ID _)") (autoload 'calibredb-org-complete-link "calibredb-org" "Define completion for Org \"calibredb:\" links.
The optional PREFIX argument is ignored.
Please notice: `calibredb-id-width' must >= the real id lenth.

(fn &optional PREFIX)") (register-definition-prefixes "calibredb-org" '("calibredb-org-")) (register-definition-prefixes "calibredb-search" '("calibredb-")) (register-definition-prefixes "calibredb-show" '("calibredb-")) (register-definition-prefixes "calibredb-transient" '("calibredb-")) (autoload 'calibredb-list "calibredb-utils" "Generate an org buffer which contain all ebooks' cover image, title and the file link." t) (register-definition-prefixes "calibredb-utils" '("calibredb-")) (provide 'calibredb-autoloads)) "map" ((map map-autoloads) (register-definition-prefixes "map" '("map-")) (provide 'map-autoloads)) "tomelr" ((tomelr tomelr-autoloads) (register-definition-prefixes "tomelr" '("tomelr-")) (provide 'tomelr-autoloads)) "ox-hugo" ((ox-hugo-deprecated ox-hugo ox-blackfriday ox-hugo-autoloads ox-hugo-pandoc-cite org-hugo-auto-export-mode) (autoload 'org-hugo-auto-export-mode "org-hugo-auto-export-mode" "Toggle auto exporting the Org file using `ox-hugo'.

This is a minor mode.  If called interactively, toggle the
`Org-Hugo-Auto-Export mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-hugo-auto-export-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "org-hugo-auto-export-mode" '("org-hugo-export-wim-to-md-after-save")) (autoload 'org-blackfriday-export-as-markdown "ox-blackfriday" "Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the heading properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org BLACKFRIDAY Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-blackfriday-convert-region-to-md "ox-blackfriday" "Convert text in the current region to Blackfriday Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it." t) (autoload 'org-blackfriday-export-to-markdown "ox-blackfriday" "Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the heading properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-blackfriday-publish-to-blackfriday "ox-blackfriday" "Publish an Org file to Blackfriday Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name.

(fn PLIST FILENAME PUB-DIR)") (register-definition-prefixes "ox-blackfriday" '("org-blackfriday-")) (put 'org-hugo-base-dir 'safe-local-variable 'stringp) (put 'org-hugo-goldmark 'safe-local-variable 'booleanp) (put 'org-hugo-section 'safe-local-variable 'stringp) (put 'org-hugo-front-matter-format 'safe-local-variable 'stringp) (put 'org-hugo-footer 'safe-local-variable 'stringp) (put 'org-hugo-preserve-filling 'safe-local-variable 'booleanp) (put 'org-hugo-delete-trailing-ws 'safe-local-variable 'booleanp) (put 'org-hugo-use-code-for-kbd 'safe-local-variable 'booleanp) (put 'org-hugo-allow-spaces-in-tags 'safe-local-variable 'booleanp) (put 'org-hugo-prefer-hyphen-in-tags 'safe-local-variable 'booleanp) (put 'org-hugo-auto-set-lastmod 'safe-local-variable 'booleanp) (put 'org-hugo-suppress-lastmod-period 'safe-local-variable 'floatp) (put 'org-hugo-export-with-toc 'safe-local-variable (lambda (x) (or (booleanp x) (integerp x)))) (put 'org-hugo-export-with-section-numbers 'safe-local-variable (lambda (x) (or (booleanp x) (equal 'onlytoc x) (integerp x)))) (put 'org-hugo-default-static-subdirectory-for-externals 'safe-local-variable 'stringp) (put 'org-hugo-export-creator-string 'safe-local-variable 'stringp) (put 'org-hugo-date-format 'safe-local-variable 'stringp) (put 'org-hugo-paired-shortcodes 'safe-local-variable 'stringp) (put 'org-hugo-link-desc-insert-type 'safe-local-variable 'booleanp) (put 'org-hugo-container-element 'safe-local-variable 'stringp) (autoload 'org-hugo-slug "ox-hugo" "Convert string STR to a `slug' and return that string.

A `slug' is the part of a URL which identifies a particular page
on a website in an easy to read form.

Example: If STR is \"My First Post\", it will be converted to a
slug \"my-first-post\", which can become part of an easy to read
URL like \"https://example.com/posts/my-first-post/\".

In general, STR is a string.  But it can also be a string with
Markdown markup because STR is often a post's sub-heading (which
can contain bold, italics, link, etc markup).

The `slug' generated from that STR follows these rules:

- Contain only lower case alphabet, number and hyphen characters
  ([[:alnum:]-]).
- Not have *any* HTML tag like \"<code>..</code>\",
  \"<span class=..>..</span>\", etc.
- Not contain any URLs (if STR happens to be a Markdown link).
- Replace \".\" in STR with \"dot\", \"&\" with \"and\",
  \"+\" with \"plus\".
- Replace parentheses with double-hyphens.  So \"foo (bar) baz\"
  becomes \"foo--bar--baz\".
- Replace non [[:alnum:]-] chars with spaces, and then one or
  more consecutive spaces with a single hyphen.
- If ALLOW-DOUBLE-HYPHENS is non-nil, at most two consecutive
  hyphens are allowed in the returned string, otherwise consecutive
  hyphens are not returned.
- No hyphens allowed at the leading or trailing end of the slug.

(fn STR &optional ALLOW-DOUBLE-HYPHENS)") (autoload 'org-hugo-export-as-md "ox-hugo" "Export current buffer to a Hugo-compatible Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the heading properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Hugo Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil.

Return the buffer the export happened to.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-hugo-export-to-md "ox-hugo" "Export current buffer to a Hugo-compatible Markdown file.

This is the main exporting function which is called by both
`org-hugo--export-file-to-md' and
`org-hugo--export-subtree-to-md', and thus
`org-hugo-export-wim-to-md' too.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the heading properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-hugo-export-wim-to-md "ox-hugo" "Export the current subtree/all subtrees/current file to a Hugo post.

This is an Export \"What I Mean\" function:

- If the current subtree has the \"EXPORT_FILE_NAME\" property,
  export only that subtree.  Return the return value of
  `org-hugo--export-subtree-to-md'.

- If the current subtree doesn't have that property, but one of
  its parent subtrees has, export from that subtree's scope.
  Return the return value of `org-hugo--export-subtree-to-md'.

- If there are no valid Hugo post subtrees (that have the
  \"EXPORT_FILE_NAME\" property) in the Org buffer the subtrees
  have that property, do file-based
  export (`org-hugo--export-file-to-md'), regardless of the value
  of ALL-SUBTREES.  Return the return value of
  `org-hugo--export-file-to-md'.

- If ALL-SUBTREES is non-nil and the Org buffer has at least 1
  valid Hugo post subtree, export all those valid post subtrees.
  Return a list of output files.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

The optional argument NOERROR is passed to
`org-hugo--export-file-to-md'.

(fn &optional ALL-SUBTREES ASYNC VISIBLE-ONLY NOERROR)" t) (autoload 'org-hugo-debug-info "ox-hugo" "Get Emacs, Org and Hugo version and ox-hugo customization info.
The information is converted to Markdown format and copied to the
kill ring.  The same information is displayed in the Messages
buffer and returned as a string in Org format." t) (register-definition-prefixes "ox-hugo" '("org-hugo-")) (register-definition-prefixes "ox-hugo-deprecated" '("org-hugo-")) (register-definition-prefixes "ox-hugo-pandoc-cite" '("org-hugo-pandoc-cite-")) (provide 'ox-hugo-autoloads)) "pulsar" ((pulsar-autoloads pulsar) (autoload 'pulsar-pulse-line "pulsar" "Temporarily highlight the current line.
When `pulsar-pulse' is non-nil (the default) make the highlight
pulse before fading away.  The pulse effect is controlled by
`pulsar-delay' and `pulsar-iterations'.

Also see `pulsar-highlight-line' for a highlight without the
pulse effect." t) (autoload 'pulsar-pulse-region "pulsar" "Temporarily highlight the active region if any.  Do nothing otherwise.
When `pulsar-pulse' is non-nil (the default) make the highlight
pulse before fading away.  The pulse effect is controlled by
`pulsar-delay' and `pulsar-iterations'.

NB:  If multiple regions are active, only the first one is impacted." t) (autoload 'pulsar-highlight-line "pulsar" "Temporarily highlight the current line.
Unlike `pulsar-pulse-line', never pulse the current line.  Keep
the highlight in place until another command is invoked.

Use `pulsar-highlight-face' (it is the same as `pulsar-face' by
default)." t) (autoload 'pulsar-define-pulse-with-face "pulsar" "Produce function to `pulsar--pulse' with FACE.
If FACE starts with the `pulsar-' prefix, remove it and keep only
the remaining text.  The assumption is that something like
`pulsar-red' will be convered to `red', thus deriving a function
named `pulsar-pulse-line-red'.  Any other FACE is taken as-is.

(fn FACE)" nil t) (function-put 'pulsar-define-pulse-with-face 'lisp-indent-function 'function) (autoload 'pulsar-highlight-dwim "pulsar" "Temporarily highlight the current line or active region.
The region may also be a rectangle.

For lines, do the same as `pulsar-highlight-line'." t) (put 'pulsar-global-mode 'globalized-minor-mode t) (defvar pulsar-global-mode nil "Non-nil if Pulsar-Global mode is enabled.
See the `pulsar-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pulsar-global-mode'.") (custom-autoload 'pulsar-global-mode "pulsar" nil) (autoload 'pulsar-global-mode "pulsar" "Toggle Pulsar mode in all buffers.
With prefix ARG, enable Pulsar-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Pulsar mode is enabled in all buffers where `pulsar--on' would do it.

See `pulsar-mode' for more information on Pulsar mode.

(fn &optional ARG)" t) (register-definition-prefixes "pulsar" '("pulsar-")) (provide 'pulsar-autoloads)) "multi-eshell" ((multi-eshell multi-eshell-autoloads) (autoload 'multi-eshell-go-back "multi-eshell" "Switch to buffer multi-eshell-last-buffer.

(fn &optional IGNORED)" t) (autoload 'multi-eshell-switch "multi-eshell" "If current buffer is not an multi-eshell, switch to current multi-eshell buffer. Otherwise, switch to next multi-eshell buffer.

(fn &optional IGNORED)" t) (autoload 'multi-eshell "multi-eshell" "Creates a shell buffer. If one already exists, this creates a new buffer, with the name '*shell*<n>', where n is chosen by the function generate-new-buffer-name.

(fn &optional NUMSHELLS)" t) (register-definition-prefixes "multi-eshell" '("if-void" "multi-eshell-" "shell-with-name")) (provide 'multi-eshell-autoloads)) "emacsql" ((emacsql-mysql emacsql-compiler emacsql-sqlite-common emacsql emacsql-sqlite-module emacsql-autoloads emacsql-sqlite emacsql-sqlite-builtin emacsql-pg emacsql-psql) (autoload 'emacsql-show-last-sql "emacsql" "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

(fn &optional PREFIX)" t) (register-definition-prefixes "emacsql" '("emacsql-")) (register-definition-prefixes "emacsql-compiler" '("emacsql-")) (register-definition-prefixes "emacsql-mysql" '("emacsql-mysql-")) (register-definition-prefixes "emacsql-pg" '("emacsql-pg-connection")) (register-definition-prefixes "emacsql-psql" '("emacsql-psql-")) (register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-")) (register-definition-prefixes "emacsql-sqlite-builtin" '("emacsql-sqlite-builtin-connection")) (register-definition-prefixes "emacsql-sqlite-common" '("emacsql-")) (register-definition-prefixes "emacsql-sqlite-module" '("emacsql-sqlite-module-connection")) (provide 'emacsql-autoloads)) "closql" ((closql-autoloads closql) (register-definition-prefixes "closql" '("closql-" "eieio-")) (provide 'closql-autoloads)) "treepy" ((treepy treepy-autoloads) (register-definition-prefixes "treepy" '("treepy-")) (provide 'treepy-autoloads)) "ghub" ((buck ghub-pkg gtea ghub ghub-graphql gogs glab ghub-autoloads gsexp) (register-definition-prefixes "buck" '("buck-default-host")) (autoload 'ghub-clear-caches "ghub" "Clear all caches that might negatively affect Ghub.

If a library that is used by Ghub caches incorrect information
such as a mistyped password, then that can prevent Ghub from
asking the user for the correct information again.

Set `url-http-real-basic-auth-storage' to nil
and call `auth-source-forget+'." t) (register-definition-prefixes "ghub" '("ghub-")) (register-definition-prefixes "ghub-graphql" '("ghub-")) (register-definition-prefixes "glab" '("glab-default-host")) (register-definition-prefixes "gogs" '("gogs-default-host")) (register-definition-prefixes "gsexp" '("gsexp-")) (register-definition-prefixes "gtea" '("gtea-default-host")) (provide 'ghub-autoloads)) "markdown-mode" ((markdown-mode markdown-mode-autoloads) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

This is a minor mode.  If called interactively, toggle the
`Markdown-Live-Preview mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `markdown-live-preview-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown")) (provide 'markdown-mode-autoloads)) "yaml" ((yaml yaml-autoloads) (register-definition-prefixes "yaml" '("yaml-")) (provide 'yaml-autoloads)) "forge" ((forge-db forge-gogs forge-bitbucket forge-gitea forge-post forge-issue forge-github forge-list forge-topic forge-autoloads forge-commands forge-pkg forge-core forge-notify forge-gitlab forge forge-repo forge-pullreq forge-revnote forge-semi) (defvar forge-add-default-bindings t "Whether to add Forge's bindings to various Magit keymaps.

If you want to disable this, then you must set this to nil before
`magit' is loaded.  If you do it before `forge' but after `magit'
is loaded, then `magit-mode-map' ends up being modified anyway.

If this is nil, then `forge-toggle-display-in-status-buffer' can
no longer do its job.  It might be better to set the global value
of `forge-display-in-status-buffer' to nil instead.  That way you
can still display topics on demand in the status buffer.") (with-eval-after-load 'magit-mode (when forge-add-default-bindings (keymap-set magit-mode-map "'" #'forge-dispatch) (keymap-set magit-mode-map "N" #'forge-dispatch) (keymap-set magit-mode-map "<remap> <magit-browse-thing>" #'forge-browse) (keymap-set magit-mode-map "<remap> <magit-copy-thing>" #'forge-copy-url-at-point-as-kill))) (with-eval-after-load 'git-commit (when forge-add-default-bindings (keymap-set git-commit-mode-map "C-c C-v" #'forge-visit-topic))) (register-definition-prefixes "forge" '("forge-")) (register-definition-prefixes "forge-bitbucket" '("forge-bitbucket-repository")) (autoload 'forge-dispatch "forge-commands" nil t) (autoload 'forge-pull "forge-commands" "Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'.

(fn &optional REPO UNTIL)" t) (autoload 'forge-pull-notifications "forge-commands" "Fetch notifications for all repositories from the current forge." t) (autoload 'forge-pull-topic "forge-commands" "Read a TOPIC and pull data about it from its forge.

(fn TOPIC)" t) (autoload 'forge-browse-issues "forge-commands" "Visit the current repository's issues using a browser." t) (autoload 'forge-browse-pullreqs "forge-commands" "Visit the current repository's pull-requests using a browser." t) (autoload 'forge-browse-topic "forge-commands" "Read a TOPIC and visit it using a browser.
By default only offer open topics but with a prefix argument
also offer closed topics.

(fn TOPIC)" t) (autoload 'forge-browse-issue "forge-commands" "Read an ISSUE and visit it using a browser.
By default only offer open issues but with a prefix argument
also offer closed issues.

(fn ISSUE)" t) (autoload 'forge-browse-pullreq "forge-commands" "Read a PULL-REQUEST and visit it using a browser.
By default only offer open pull-requests but with a prefix
argument also offer closed pull-requests.

(fn PULL-REQUEST)" t) (autoload 'forge-browse-commit "forge-commands" "Read a COMMIT and visit it using a browser.

(fn COMMIT)" t) (autoload 'forge-browse-branch "forge-commands" "Read a BRANCH and visit it using a browser.

(fn BRANCH)" t) (autoload 'forge-browse-remote "forge-commands" "Read a REMOTE and visit it using a browser.

(fn REMOTE)" t) (autoload 'forge-browse-repository "forge-commands" "Read a REPOSITORY and visit it using a browser.

(fn REPOSITORY)" t) (autoload 'forge-browse-this-topic "forge-commands" "Visit the topic at point using a browser." t) (autoload 'forge-browse-this-repository "forge-commands" "Visit the repository at point using a browser." t) (autoload 'forge-copy-url-at-point-as-kill "forge-commands" "Copy the url of the thing at point." t) (autoload 'forge-browse "forge-commands" "Visit the thing at point using a browser." t) (autoload 'forge-visit-topic "forge-commands" "Read a TOPIC and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics.

(fn TOPIC)" t) (autoload 'forge-visit-issue "forge-commands" "Read an ISSUE and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics.

(fn ISSUE)" t) (autoload 'forge-visit-pullreq "forge-commands" "Read a PULL-REQUEST and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics.

(fn PULL-REQUEST)" t) (autoload 'forge-visit-this-topic "forge-commands" "Visit the topic at point." t) (autoload 'forge-visit-this-repository "forge-commands" "Visit the repository at point." t) (autoload 'forge-branch-pullreq "forge-commands" "Create and configure a new branch from a pull-request.
Please see the manual for more information.

(fn PULLREQ)" t) (autoload 'forge-checkout-pullreq "forge-commands" "Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information.

(fn PULLREQ)" t) (autoload 'forge-checkout-worktree "forge-commands" "Create, configure and checkout a new worktree from a pull-request.
This is like `forge-checkout-pullreq', except that it also
creates a new worktree. Please see the manual for more
information.

(fn PATH PULLREQ)" t) (autoload 'forge-fork "forge-commands" "Fork the current repository to FORK and add it as a REMOTE.
If the fork already exists, then that isn't an error; the remote
is added anyway.  Currently this only supports Github and Gitlab.

(fn FORK REMOTE)" t) (autoload 'forge-merge "forge-commands" "Merge the current pull-request using METHOD using the forge's API.

If there is no current pull-request or with a prefix argument,
then read pull-request PULLREQ to visit instead.

Use of this command is discouraged.  Unless the remote repository
is configured to disallow that, you should instead merge locally
and then push the target branch.  Forges detect that you have
done that and respond by automatically marking the pull-request
as merged.

(fn PULLREQ METHOD)" t) (autoload 'forge-rename-default-branch "forge-commands" "Rename the default branch to NEWNAME.
Change the name on the upstream remote and locally, and update
the upstream remotes of local branches accordingly." t) (autoload 'forge-add-pullreq-refspec "forge-commands" nil t) (autoload 'forge-add-repository "forge-commands" nil t) (autoload 'forge-add-user-repositories "forge-commands" "Add all of USER's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment.

(fn HOST USER)" t) (autoload 'forge-add-organization-repositories "forge-commands" "Add all of ORGANIZATION's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment.

(fn HOST ORGANIZATION)" t) (autoload 'forge-remove-repository "forge-commands" "Remove a repository from the database.

(fn HOST OWNER NAME)" t) (autoload 'forge-remove-topic-locally "forge-commands" "Remove a topic from the local database only.
Due to how the supported APIs work, it would be too expensive to
automatically remove topics from the local database that were
removed from the forge.  The purpose of this command is to allow
you to manually clean up the local database.

(fn TOPIC)" t) (autoload 'forge-reset-database "forge-commands" "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development." t) (register-definition-prefixes "forge-commands" '("forge-")) (register-definition-prefixes "forge-core" '("forge-")) (register-definition-prefixes "forge-db" '("forge-")) (register-definition-prefixes "forge-gitea" '("forge-gitea-repository")) (register-definition-prefixes "forge-github" '("forge-")) (register-definition-prefixes "forge-gitlab" '("forge-gitlab-repository")) (register-definition-prefixes "forge-gogs" '("forge-gogs-repository")) (register-definition-prefixes "forge-issue" '("forge-")) (autoload 'forge-list-topics "forge-list" "List topics of the current repository in a separate buffer.

(fn ID)" t) (autoload 'forge-list-issues "forge-list" "List issues of the current repository in a separate buffer.

(fn ID)" t) (autoload 'forge-list-labeled-issues "forge-list" "List issues of the current repository that have LABEL.
List them in a separate buffer.

(fn ID LABEL)" t) (autoload 'forge-list-assigned-issues "forge-list" "List issues of the current repository that are assigned to you.
List them in a separate buffer.

(fn ID)" t) (autoload 'forge-list-owned-issues "forge-list" "List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t) (autoload 'forge-list-pullreqs "forge-list" "List pull-requests of the current repository in a separate buffer.

(fn ID)" t) (autoload 'forge-list-labeled-pullreqs "forge-list" "List pull-requests of the current repository that have LABEL.
List them in a separate buffer.

(fn ID LABEL)" t) (autoload 'forge-list-assigned-pullreqs "forge-list" "List pull-requests of the current repository that are assigned to you.
List them in a separate buffer.

(fn ID)" t) (autoload 'forge-list-requested-reviews "forge-list" "List pull-requests of the current repository that are awaiting your review.
List them in a separate buffer.

(fn ID)" t) (autoload 'forge-list-owned-pullreqs "forge-list" "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t) (autoload 'forge-list-authored-pullreqs "forge-list" "List open pull-requests of the current repository that are authored by you.
List them in a separate buffer.

(fn ID)" t) (autoload 'forge-list-authored-issues "forge-list" "List open issues from the current repository that are authored by you.
List them in a separate buffer.

(fn ID)" t) (autoload 'forge-list-notifications "forge-list" "List notifications." t) (autoload 'forge-list-repositories "forge-list" "List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database." t) (autoload 'forge-list-owned-repositories "forge-list" "List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t) (register-definition-prefixes "forge-list" '("forge-")) (register-definition-prefixes "forge-notify" '("forge-")) (register-definition-prefixes "forge-post" '("forge-")) (register-definition-prefixes "forge-pullreq" '("forge-")) (register-definition-prefixes "forge-repo" '("forge-")) (register-definition-prefixes "forge-revnote" '("forge-revnote")) (register-definition-prefixes "forge-semi" '("forge-")) (register-definition-prefixes "forge-topic" '("forge-")) (provide 'forge-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 15 "melpa" nil "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "diminish" nil "company" nil "gcmh" nil "objed" nil "cl-lib" nil "tool-bar" nil "solaire-mode" nil "doom-themes" nil "doom-themes-ext-treemacs" nil "doom-modeline" nil "compat" nil "seq" nil "nerd-icons" nil "shrink-path" nil "s" nil "dash" nil "f" nil "ef-themes" nil "palimpsest" nil "langtool" nil "rainbow-delimiters" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "org-tracktable" nil "org-sticky-header" nil "projectile" nil "time" nil "vertico" nil "savehist" nil "orderless" nil "marginalia" nil "consult" nil "night-owl-theme" nil "embark" nil "embark-consult" nil "consult-company" nil "crux" nil "company-wordfreq" nil "eros" nil "adoc-mode" nil "which-key" nil "magit" nil "git-commit" nil "transient" nil "with-editor" nil "magit-section" nil "lispy" nil "ace-window" nil "avy" nil "iedit" nil "swiper" nil "ivy" nil "hydra" nil "lv" nil "zoutline" nil "git-gutter" nil "poet-theme" nil "org-modern" nil "centered-cursor-mode" nil "olivetti" nil "beacon" nil "mixed-pitch" nil "flyspell" nil "flycheck" nil "pkg-info" nil "epl" nil "let-alist" nil "consult-flyspell" nil "company-posframe" nil "posframe" nil "yasnippets" nil "company-box" nil "frame-local" nil "hyperbole" nil "vertico-posframe" nil "org-appear" nil "eshell-git-prompts" nil "eshell-git-prompt" nil "eshell-syntax-highlighting" nil "company-shell" nil "pdf-tools" nil "tablist" nil "nov" nil "esxml" nil "kv" nil "ox-pandoc" nil "ht" nil "org-view-mode" nil "calibredb" nil "request" nil "ox-hugo" nil "tomelr" nil "map" nil "pulsar" nil "multi-eshell" nil "custom-macros" nil "forge" nil "closql" nil "emacsql" nil "ghub" nil "treepy" nil "markdown-mode" nil "yaml" nil)) melpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "diminish" (diminish :type git :flavor melpa :host github :repo "myrjola/diminish.el") "company" (company :type git :flavor melpa :files (:defaults "icons" ("images/small" "doc/images/small/*.png") "company-pkg.el") :host github :repo "company-mode/company-mode") "gcmh" (gcmh :type git :flavor melpa :host gitlab :repo "koral/gcmh") "objed" (objed :type git :flavor melpa :host github :repo "clemera/objed") "cl-lib" nil "solaire-mode" (solaire-mode :type git :flavor melpa :host github :repo "hlissner/emacs-solaire-mode") "doom-themes" (doom-themes :type git :flavor melpa :files (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el" "doom-themes-pkg.el") :host github :repo "doomemacs/themes") "doom-themes-ext-treemacs" nil "doom-modeline" (doom-modeline :type git :flavor melpa :host github :repo "seagle0128/doom-modeline") "compat" nil "seq" nil "nerd-icons" (nerd-icons :type git :flavor melpa :files (:defaults "data" "nerd-icons-pkg.el") :host github :repo "rainstormstudio/nerd-icons.el") "shrink-path" (shrink-path :type git :flavor melpa :host gitlab :repo "bennya/shrink-path.el") "s" (s :type git :flavor melpa :host github :repo "magnars/s.el") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "f" (f :type git :flavor melpa :host github :repo "rejeep/f.el") "ef-themes" nil "palimpsest" (palimpsest :type git :flavor melpa :host github :repo "danielsz/Palimpsest") "langtool" (langtool :type git :flavor melpa :files ("langtool.el" "langtool-pkg.el") :host github :repo "mhayashi1120/Emacs-langtool") "rainbow-delimiters" (rainbow-delimiters :type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters") "org-tracktable" (org-tracktable :type git :flavor melpa :host github :repo "tty-tourist/org-tracktable") "org-sticky-header" (org-sticky-header :type git :flavor melpa :host github :repo "alphapapa/org-sticky-header") "projectile" (projectile :type git :flavor melpa :host github :repo "bbatsov/projectile") "time" nil "vertico" (vertico :type git :flavor melpa :files (:defaults "extensions/vertico-*.el" "vertico-pkg.el") :host github :repo "minad/vertico") "savehist" nil "orderless" (orderless :type git :flavor melpa :host github :repo "oantolin/orderless") "marginalia" (marginalia :type git :flavor melpa :host github :repo "minad/marginalia") "consult" (consult :type git :flavor melpa :host github :repo "minad/consult") "night-owl-theme" (night-owl-theme :type git :flavor melpa :host github :repo "aaronjensen/night-owl-emacs") "embark" (embark :type git :flavor melpa :files ("embark.el" "embark-org.el" "embark.texi" "embark-pkg.el") :host github :repo "oantolin/embark") "embark-consult" (embark-consult :type git :flavor melpa :files ("embark-consult.el" "embark-consult-pkg.el") :host github :repo "oantolin/embark") "consult-company" (consult-company :type git :flavor melpa :host github :repo "mohkale/consult-company") "crux" (crux :type git :flavor melpa :host github :repo "bbatsov/crux") "company-wordfreq" (company-wordfreq :type git :flavor melpa :host github :repo "johannes-mueller/company-wordfreq.el") "eros" (eros :type git :flavor melpa :host github :repo "xiongtx/eros") "adoc-mode" (adoc-mode :type git :flavor melpa :host github :repo "bbatsov/adoc-mode") "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "magit" (magit :type git :flavor melpa :files ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "magit-section" (magit-section :type git :flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi" "magit-section-pkg.el") :host github :repo "magit/magit") "lispy" (lispy :type git :flavor melpa :files (:defaults "lispy-clojure.clj" "lispy-clojure.cljs" "lispy-python.py" "lispy-pkg.el") :host github :repo "abo-abo/lispy") "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "iedit" (iedit :type git :flavor melpa :host github :repo "victorhge/iedit") "swiper" (swiper :type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper") "ivy" (ivy :type git :flavor melpa :files (:defaults "doc/ivy-help.org" (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "ivy-pkg.el") :host github :repo "abo-abo/swiper") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "zoutline" (zoutline :type git :flavor melpa :host github :repo "abo-abo/zoutline") "git-gutter" (git-gutter :type git :flavor melpa :host github :repo "emacsorphanage/git-gutter") "poet-theme" (poet-theme :type git :flavor melpa :host github :repo "kunalb/poet") "org-modern" (org-modern :type git :flavor melpa :host github :repo "minad/org-modern") "centered-cursor-mode" (centered-cursor-mode :type git :flavor melpa :host github :repo "andre-r/centered-cursor-mode.el") "olivetti" (olivetti :type git :flavor melpa :host github :repo "rnkn/olivetti") "beacon" (beacon :type git :flavor melpa :host github :repo "Malabarba/beacon") "mixed-pitch" (mixed-pitch :type git :flavor melpa :host gitlab :repo "jabranham/mixed-pitch") "flyspell" nil "flycheck" (flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck") "pkg-info" (pkg-info :type git :flavor melpa :host github :repo "emacsorphanage/pkg-info") "epl" (epl :type git :flavor melpa :host github :repo "cask/epl") "let-alist" nil "consult-flyspell" (consult-flyspell :type git :flavor melpa :host gitlab :repo "OlMon/consult-flyspell") "company-posframe" (company-posframe :type git :flavor melpa :host github :repo "tumashu/company-posframe") "posframe" (posframe :type git :flavor melpa :host github :repo "tumashu/posframe") "yasnippets" nil "company-box" (company-box :type git :flavor melpa :files (:defaults "images" "company-box-pkg.el") :host github :repo "sebastiencs/company-box") "frame-local" (frame-local :type git :flavor melpa :host github :repo "sebastiencs/frame-local") "hyperbole" nil "vertico-posframe" nil "org-appear" (org-appear :type git :flavor melpa :host github :repo "awth13/org-appear") "eshell-git-prompts" nil "eshell-git-prompt" (eshell-git-prompt :type git :flavor melpa :host github :repo "xuchunyang/eshell-git-prompt") "eshell-syntax-highlighting" (eshell-syntax-highlighting :type git :flavor melpa :host github :repo "akreisher/eshell-syntax-highlighting") "company-shell" (company-shell :type git :flavor melpa :host github :repo "Alexander-Miller/company-shell") "pdf-tools" (pdf-tools :type git :flavor melpa :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools") "tablist" (tablist :type git :flavor melpa :host github :repo "emacsorphanage/tablist") "nov" (nov :type git :flavor melpa :repo "https://depp.brause.cc/nov.el.git") "esxml" (esxml :type git :flavor melpa :files ("esxml.el" "esxml-query.el" "esxml-pkg.el") :host github :repo "tali713/esxml") "kv" (kv :type git :flavor melpa :host github :repo "nicferrier/emacs-kv") "ox-pandoc" (ox-pandoc :type git :flavor melpa :host github :repo "emacsorphanage/ox-pandoc") "ht" (ht :type git :flavor melpa :host github :repo "Wilfred/ht.el") "org-view-mode" (org-view-mode :type git :flavor melpa :host github :repo "amno1/org-view-mode") "calibredb" (calibredb :type git :flavor melpa :host github :repo "chenyanming/calibredb.el") "request" (request :type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request") "ox-hugo" (ox-hugo :type git :flavor melpa :host github :repo "kaushalmodi/ox-hugo") "tomelr" nil "map" nil "pulsar" nil "multi-eshell" nil "custom-macros" nil "forge" (forge :type git :flavor melpa :host github :repo "magit/forge") "closql" (closql :type git :flavor melpa :host github :repo "magit/closql") "emacsql" (emacsql :type git :flavor melpa :files (:defaults "sqlite" "emacsql-pkg.el") :host github :repo "magit/emacsql") "ghub" (ghub :type git :flavor melpa :host github :repo "magit/ghub") "treepy" (treepy :type git :flavor melpa :host github :repo "volrath/treepy.el") "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "yaml" (yaml :type git :flavor melpa :host github :repo "zkry/yaml.el"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "nongnu-elpa" nil "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "tool-bar" nil "doom-themes-ext-treemacs" nil "compat" (compat :type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git"))) "seq" nil "ef-themes" (ef-themes :type git :host github :repo "emacs-straight/ef-themes" :files ("*" (:exclude ".git"))) "time" nil "savehist" nil "flyspell" nil "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "yasnippets" nil "hyperbole" (hyperbole :type git :host github :repo "emacs-straight/hyperbole" :files ("*" (:exclude ".git"))) "vertico-posframe" (vertico-posframe :type git :host github :repo "emacs-straight/vertico-posframe" :files ("*" (:exclude ".git"))) "eshell-git-prompts" nil "tomelr" (tomelr :type git :host github :repo "emacs-straight/tomelr" :files ("*" (:exclude ".git"))) "map" (map :type git :host github :repo "emacs-straight/map" :files ("*" (:exclude ".git"))) "pulsar" (pulsar :type git :host github :repo "emacs-straight/pulsar" :files ("*" (:exclude ".git"))) "multi-eshell" nil "custom-macros" nil)) nongnu-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 4 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "doom-themes-ext-treemacs" nil "seq" nil "time" nil "savehist" nil "flyspell" nil "yasnippets" nil "eshell-git-prompts" nil "multi-eshell" nil "custom-macros" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "tool-bar" nil "doom-themes-ext-treemacs" nil "seq" nil "time" nil "savehist" nil "flyspell" nil "yasnippets" `(yasnippets :type git :host github :repo "rejeep/yasnippets" :files (:defaults)) "eshell-git-prompts" nil "multi-eshell" nil "custom-macros" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil "doom-themes-ext-treemacs" nil "seq" nil "time" nil "savehist" nil "flyspell" nil "eshell-git-prompts" nil "multi-eshell" (multi-eshell :type git :host github :repo "emacsmirror/multi-eshell") "custom-macros" nil))))

("yaml" "markdown-mode" "treepy" "ghub" "emacsql" "closql" "forge" "multi-eshell" "pulsar" "map" "tomelr" "ox-hugo" "request" "calibredb" "org-view-mode" "ht" "ox-pandoc" "kv" "esxml" "nov" "tablist" "pdf-tools" "company-shell" "eshell-syntax-highlighting" "eshell-git-prompt" "org-appear" "vertico-posframe" "hyperbole" "frame-local" "company-box" "posframe" "company-posframe" "consult-flyspell" "let-alist" "epl" "pkg-info" "flycheck" "flyspell" "mixed-pitch" "beacon" "olivetti" "centered-cursor-mode" "org-modern" "poet-theme" "git-gutter" "zoutline" "lv" "hydra" "ivy" "swiper" "iedit" "avy" "ace-window" "lispy" "magit-section" "with-editor" "transient" "git-commit" "magit" "which-key" "adoc-mode" "eros" "company-wordfreq" "crux" "consult-company" "embark-consult" "embark" "night-owl-theme" "consult" "marginalia" "orderless" "savehist" "vertico" "time" "projectile" "org-sticky-header" "org-tracktable" "org" "rainbow-delimiters" "langtool" "palimpsest" "ef-themes" "f" "dash" "s" "shrink-path" "nerd-icons" "seq" "compat" "doom-modeline" "doom-themes" "cl-lib" "solaire-mode" "gcmh" "company" "diminish" "emacs" "straight" "emacsmirror-mirror" "el-get" "nongnu-elpa" "gnu-elpa-mirror" "melpa" "org-elpa")

t
