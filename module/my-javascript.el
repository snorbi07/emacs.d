;;; my-javascript --- module provides support for working with JavaScript
;;; Commentary:

;;; Code:

(require 'my-common)

;;; References:
;;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;; https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
;;; https://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring/

;;; Depends on external tools:
;;; tern
;;; eslint
;;; babel-eslint
;;; eslint-plugin-react
;;; Installable using: npm install -g tern eslint babel-eslint eslint-plugin-react
;;; assuming you have node installed an on your path

(install-if-missing '(flycheck
                      js2-mode
                      js2-refactor
                      json-mode
                      web-mode
		      less-css-mode
                      tern
                      company
                      company-tern
                      exec-path-from-shell))

;; use js2-mode and web-mode for .js and .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; tern setup
;; add as company mode backend
(require 'company)
(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)

;; js2-refactor setup
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode.  Adjust indent."
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'my-javascript)
;;; my-javascript ends here
