(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

(package-initialize
 (set-frame-font "Menlo:pixelsize=13"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#003f8e" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#ffffff"))
 '(beacon-color "#ff9da4")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#003f8e")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-ignore-buffer-patterns (quote ("\\.txt\\'" "\\.mkd\\'")))
 '(helm-ag-insert-at-point (quote symbol))
 '(package-selected-packages
   (quote
    (helm-themes flx-ido ag paredit xml+ bundler rubocop helm-ag-r telephone-line git-gutter mode-icons projectile-rails all-the-icons highlight-indent-guides color-theme-sanityinc-tomorrow ample-theme dockerfile-mode docker clojure-mode-extra-font-locking smartparens rainbow-blocks rainbow-delimiters cider-hydra cider clojure-snippets clojure-mode ansible ruby-test-mode column-enforce-mode auto-complete-exuberant-ctags auto-complete coffee-mode haml-mode projectile-codesearch helm-ag yaml-mode terraform-mode helm-projectile helm dracula-theme magit use-package)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'helm)

(require 'magit)

(require 'terraform-mode)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(ac-config-default)
(setq default-directory "~/dev")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(global-git-gutter-mode +1)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(telephone-line-mode 1)
(add-hook 'ruby-mode-hook #'rubocop-mode)

;projectile
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-switch-project-action #'projectile-dired)
(setq projectile-find-dir-includes-top-level t)
(projectile-mode 1)

;flx
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;helm
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(require 'helm-config)
(require 'helm-themes)

;Custom
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
