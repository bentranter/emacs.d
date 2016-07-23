;;; package --- setup-php.el

;;; Commentary:
;;; I guess I have to write PHP

;;; Code:

(require 'php-mode)
(require 'php-extras)

;; File suffixes that should use PHP mode
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$" . php-mode))

;; Autocompletion
(add-hook 'php-mode-hook 'company-mode)
(add-hook 'php-mode-hook (lambda ()
			   (set
			    (make-local-variable 'company-backends) '(php-extras-company))
	  (company-mode)))

(provide 'setup-php)

;;; setup-php.el ends here
