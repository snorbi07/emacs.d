;;; my-typescript.el --- TypeScript development environment setup
;;; Commentary:

;;; Code:
(use-package tide
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package company
  :ensure t)
(use-package typescript-mode
  :mode("\\.ts\\'" . typescript-mode)
  :ensure t)

(require 'eldoc)
(require 'typescript-mode)
(require 'web-mode)

;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(add-hook 'typescript-mode-hook
          (lambda ()
	    (message "ANYAD KURVA PICSAJAT")
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Tide can be used along with web-mode to edit tsx files

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))))


(provide 'my-typescript)
;;; my-typescript.el ends here
