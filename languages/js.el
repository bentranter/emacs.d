;;; package --- js.el

;;; Commentary:
;;; My settings for JavaScript

;;; Code:

(add-hook 'js-mode-hook (lambda ()
			  tern-mode t))
(add-hook 'js-mode-hook 'company-mode)
(add-hook 'js-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-tern))
			  (company-mode)))

(provide 'js)

;;; js.el ends here
