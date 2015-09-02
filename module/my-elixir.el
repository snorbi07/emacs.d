;;; my-elixir --- module provides support for working with Elixir 
;;; Commentary:
;;; A working installation of elixir-lang is assumed. For details check: http://elixir-lang.org/install.html

(require 'common)

(install-if-missing '(elixir-mode alchemist))

(add-hook 'elixir-mode-hook (lambda () (alchemist-mode)))

(provide 'my-elixir)
;;; my-elixir ends here
