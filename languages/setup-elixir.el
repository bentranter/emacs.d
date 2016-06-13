;;; package --- setup-elixir.el

;;; Commentary:
;;; My settings for Elixir.

;;; Code:

(require 'elixir-mode)
(require 'alchemist)

;; Use company mode with Elixir.
(add-hook 'elixir-mode-hook 'company-mode)

(provide 'setup-elixir)

;;; setup-elixir ends here
