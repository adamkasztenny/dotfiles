;;; 
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


(package-initialize)
(set-frame-font "Menlo:pixelsize=12")
(load-theme 'dracula t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "72759f4e42617df7a07d0a4f4b08982314aa97fbd495a5405c9b11f48bd6b839" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (flycheck-tip flycheck-pos-tip neotree docker-compose-mode elein scss-mode coffee-mode haml-mode discover-my-major railscasts-reloaded-theme solarized-theme telephone-line mode-icons helm-ag paredit helm-company elpy hungry-delete multiple-cursors helm-projectile projectile smex helm dracula-theme ace-popup-menu rainbow-delimiters exec-path-from-shell company company-go company-terraform flycheck flycheck-clojure flycheck-pycheckers flycheck-yamllint flymake-coffee flymake-css flymake-haml flymake-jslint flymake-json magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(package-install 'exec-path-from-shell)
;;(exec-path-from-shell-initialize)

;; fkycheck initializer
(global-flycheck-mode)
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-tip)

;; initialize company
(global-company-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;configure paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
;; remove gnu logo
;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; general configurations
(setq column-number-mode t)
(global-linum-mode)

(setq linum-format " %d ")

;; setup ace-popup-menu
(ace-popup-menu-mode 1)

;; pretiffy simbols mode
(global-prettify-symbols-mode +1)

;; helm mode
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; projectile
(projectile-mode 1)
(require 'helm-projectile)
(helm-projectile-on)

;; multi cursos
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; company
(company-quickhelp-mode)

;; python setup
(elpy-enable)
(when (require 'elpy nil t)
  (elpy-enable))

;; helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; mode icons
(mode-icons-mode)

;; telephone mode
(require 'telephone-line)
(telephone-line-mode 1)

;; remove sound
(setq ring-bell-function 'ignore)

;; Set terminal :
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))
(setq default-directory "~/dev")

(tool-bar-mode -1)

(scroll-bar-mode -1)

(toggle-frame-maximized)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

;; neotree setup
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'icons)

;;end of the file
(provide 'init)
