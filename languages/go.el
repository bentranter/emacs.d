;;; package --- go.el

;;; Commentary:
;;; My settings for Go.

;;; Code:

(require 'company-go)

(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/Users/bentranter/go/bin")

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'company-mode)                     ; Enable autocomplete for Go using company mode
(add-hook 'go-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))



(provide 'go)

;;; go.el ends here
