;;; init.el --- emacs customization

;;; Commentary: It's my emacs config duhhh

(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)             ; Look for packages in Melpa

(setq inhibit-startup-message t)                           ; Disable the default startup message

(global-linum-mode t)                                      ; Enable global line numbers
(setq linum-format "%4d \u2502 ")

(defalias 'yes-or-no-p 'y-or-n-p)                          ; Simplify prompts (only yes or no)

(setq column-number-mode t)                                ; Show cursor position

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") ; Load custom themes

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

(setq exec-path (cons "/usr/local/go/bin" exec-path))      ; Add Go binaries to path
(add-to-list 'exec-path "/Users/bentranter/Go/bin")        ; Add $GOPATH binaries to path
(add-hook 'before-save-hook 'gofmt-before-save)            ; Add `go fmt` on save hook

(global-flycheck-mode)                                     ; Enable Flycheck

(add-hook 'go-mode-hook 'company-mode)                     ; Enable autocomplete for Go using company mode
(add-hook 'go-mode-hook (lambda ()
  (set
    (make-local-variable 'company-backends) '(company-go))
    (company-mode)))

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
