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
	noctilux-theme
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

;; Setup font
(set-frame-font "Source Code Pro 13")

;; Hide toolbars and menu bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Match parentheses
(electric-pair-mode 1)

; Configure Company Mode
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))
(setq-default company-selection-wrap-around t)
(setq-default company-minimum-prefix-length 1)

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

;; Use Neotree's smart open
(setq-default neo-smart-open t)

;; Don't allow neotree to be the only open window
(setq-default neo-dont-be-alone t)

(setq neo-theme 'nerd)

(setq backup-directory-alist `(("." . "~/.saves")))        ; Place saves elsewhere

(global-linum-mode t)                                      ; Enable global line numbers
(setq linum-format "%4d  ")
(global-hl-line-mode 1)

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

(load-theme 'spacegray t)

(provide 'init)

;;; init.el ends here
