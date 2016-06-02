;;; package --- js.el

;;; Commentary:
;;; My settings for JavaScript

;;; Code:

(require 'tern)
(require 'company-tern)

(add-hook 'js-mode-hook 'js2-minor-mode)

(setq-default js2-basic-offset 2)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-tern))
			  (company-mode)))

(provide 'js)

;;; js.el ends here
