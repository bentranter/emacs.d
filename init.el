;:; package --- init.el

;;; Commentary:
;;; This is my Emacs config.  I typically use Go, JavaScript, HTML, and CSS.

;;; Code:

;; Disable startup message
(setq inhibit-startup-message t)

;; Set path to dependencies
(defvar languages-dir)
(setq languages-dir (expand-file-name "languages" user-emacs-directory))

(defvar themes-dir)
(setq themes-dir (expand-file-name "themes" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path languages-dir)
(add-to-list 'custom-theme-load-path themes-dir)

;; Keep custom file separate because it's annoying
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Setup package stuff
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; List all packages
(defvar package-list)
(setq package-list
      '(
	alchemist
	auto-complete
	cmake-ide
	company
	company-go
	company-tern
	dash
	dash-functional
	elixir-mode
	epl
	evil
	exec-path-from-shell
	flycheck
	go-autocomplete
	go-mode
	js2-mode
	neotree
	tern
	tern-auto-complete
	))

;; Refresh package list
(unless package-archive-contents
  (package-refresh-contents))

;; Install any missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load language specific stuff
(require 'lisp)
(require 'go)
(require 'setup-js)
(require 'setup-cpp)
(require 'setup-elixir)

;; Use Vim keybindings like a normal person
(require 'evil)
(evil-mode 1)

;; (require 'sr-speedbar)
;; (global-set-key (kbd "C-x C-t") 'sr-speedbar-toggle)
;; (setq sr-speedbar-width 28)
;; (setq sr-speedbar-right-side nil)

(require 'neotree)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(global-set-key (kbd "C-x C-t") 'neotree-toggle)

(setq backup-directory-alist `(("." . "~/.saves")))        ; Place saves elsewhere

(global-linum-mode t)                                      ; Enable global line numbers
(setq linum-format "%4d  ")

(defalias 'yes-or-no-p 'y-or-n-p)                          ; Simplify prompts (only yes or no)

(setq column-number-mode t)                                ; Show cursor position

(setq org-src-preserve-indentation t)                      ; Preserve indentation

(unless window-system                                      ; Enable mouse in iTerm2
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
    (interactive)
    (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
    (interactive)
    (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(global-flycheck-mode)                                     ; Enable Flycheck

(menu-bar-mode 0)                                          ; No file/edit/blabla top menu
(setq inhibit-startup-message t)                           ; Disbale startup messages
(setq bell-volume 0)                                       ; No more terminal bell

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#bf616a" "#B4EB89" "#ebcb8b" "#89AAEB" "#C189EB" "#89EBCA" "#232830"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "46ad2496adcda77b6dbc62c9c5bc0d4eee6317e9cb74c6ddd6b226936e808159" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "306b69248b023217a5963262363dc7cc10bfeb35e9e13971ac9e79ea3cb91d34" default)))
 '(fci-rule-color "#343d46")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)

;;; init.el ends here
