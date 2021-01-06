;; My init file for an awesome emacs.

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'packages)
(require 'my-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#222222" "#a85454" "#65a854" "#8d995c" "#5476a8" "#9754a8" "#54a8a8" "#969696"])
 '(ansi-term-color-vector
   [unspecified "#1d1f21" "#cc342b" "#198844" "#fba922" "#3971ed" "#a36ac7" "#3971ed" "#c5c8c6"] t)
 '(beacon-color "#ff9da4")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "8c3ed254d0fd7829725d904cf0adc7f4de2d72ec7b7b19e8d8cdd797eda4e2c6" "9ab46512b8b69478b057f79d0eb0faee61d19e53917a868a14e41bf357cee6d4" "6313eeb08a54045a6d3945c28ab5a97916334e189cebd9c67c8b72beed0de265" "efd849c804148b88536914ccdee08285fd7376e2e3334522c9afc00fd7e594da" "4363ac3323e57147141341a629a19f1398ea4c0b25c79a6661f20ffc44fdd2cb" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "4e764943cc022ba136b80fa82d7cdd6b13a25023da27528a59ac61b0c4f1d16f" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "c59ed2bceca3ba0c01a7689cb9067c9b7f11924aaad98ed4b0c1f0818e542a92" "379a804655efccc13a3d446468992bfdfc30ff27d19cfda6f62c7f9c9e7a8a7d" "9f15d03580b08dae41a1e5c1f00d1f1aa99fea121ca32c28e2abec9563c6e32c" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "56911bd75304fdb19619c9cb4c7b0511214d93f18e566e5b954416756a20cc80" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "e30e72b10b9c7887ff8adcd1a25b5c6eaa32665e0f8f40994e5b6d51069d3b2a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "7675ffd2f5cb01a7aab53bcdd702fa019b56c764900f2eea0f74ccfc8e854386" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "487cfc75d735a297baceb6e531b3de722a587ae73b301250382964d9ae13939f" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" default))
 '(electric-pair-mode t)
 '(fci-rule-color "#073642")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-posframe-border-width 5)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#4ab61e4953c5" "#24102e386af6" "#54ef0d7751a2" "#1e79173c7be9" "#40d020d85306" "#54ef0d7751a2" "#104023987c14"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hydra-posframe-show-params
   '(:poshandler posframe-poshandler-frame-bottom-center :internal-border-width 15 :internal-border-color "#222222" :background-color "#222222") t)
 '(ivy-posframe-border-width 15)
 '(ivy-posframe-style 'frame-bottom-window-center)
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(ledger-reports
   '(("b" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#dc322f")
 '(objed-mode nil nil (objed))
 '(org-fontify-whole-block-delimiter-line t)
 '(org-fontify-whole-heading-line t)
 '(pdf-view-midnight-colors (cons "#556b72" "#FDF6E3"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c4c075980000")
     (60 . "#ba837ef50000")
     (80 . "#b58900")
     (100 . "#a28d87c90000")
     (120 . "#9be887d10000")
     (140 . "#953287c80000")
     (160 . "#8e6887af0000")
     (180 . "#859900")
     (200 . "#76f495013bd5")
     (220 . "#6b039bb14fa2")
     (240 . "#5adca25c6284")
     (260 . "#4293a905750f")
     (280 . "#2aa198")
     (300 . "#247b9fbfa83b")
     (320 . "#24bd97b8b831")
     (340 . "#1d0e8fa5c80a")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(window-divider-default-right-width 1)
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
