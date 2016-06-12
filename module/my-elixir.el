;;; my-elixir --- module provides support for working with Elixir
;;; Commentary:
;;; A working installation of elixir-lang is assumed. For details check: http://elixir-lang.org/install.html

;;; Code:

(require 'my-common)
(require 'smartparens)
(require 'use-package)
;; called by init.el, so commented out here for now
;; (require 'smartparens-config)

(use-package elixir-yasnippets
  :ensure t)

(install-if-missing '(elixir-mode alchemist smartparens))

(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook (lambda () (alchemist-mode)))
(add-hook 'elixir-mode-hook (lambda () (smartparens-mode)))
(add-hook 'elixir-mode-hook (lambda () (yas-minor-mode)))


(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(provide 'my-elixir)
;;; my-elixir ends here
