;;; package --- setup-js.el

;;; Commentary:
;;; My settings for JavaScript

;;; Code:

(require 'tern)
(require 'company-tern)

;(add-to-list 'auto-mode-alist '("\\.js$\\'" . js2-mode))

;(setq-default js2-basic-offset 2)
;(setq-default js2-show-parse-errors nil)
;(setq-default js2-strict-missing-semi-warning nil)
;(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

;(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js-mode-hook 'company-mode)
(add-hook 'js-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-tern))
			  (company-mode)))

(setq js-indent-level 2)

(provide 'setup-js)

;;; setup-js.el ends here
