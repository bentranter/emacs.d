;;; package --- js.el

;;; Commentary:
;;; My settings for JavaScript

;;; Code:

(require 'tern)
(require 'company-tern)

(add-hook 'js-mode-hook 'company-mode)
(add-hook 'js-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-tern))
			  (company-mode)))

(provide 'js)

;;; js.el ends here
