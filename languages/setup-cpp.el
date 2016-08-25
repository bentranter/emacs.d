;;; package --- setup-cpp.el

;;; Commentary:
;;; My settings for C++.

;;; Code:

;; (require 'irony)
(require 'cmake-ide)
;; (require 'company-irony)

(cmake-ide-setup)

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; (add-hook 'irony-mode 'company-mode)
;; (add-hook 'irony-mode (lambda ()
;; 			(set
;; 			 (make-local-variable 'company-backends) '(company-irony))
;; 			(company-mode)))

(provide 'setup-cpp)

;;; setup-cpp.el ends here
