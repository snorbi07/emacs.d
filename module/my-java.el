;;; my-java --- module provides support for working with Java

;;; Commentary:

;;; Code:

;;; References:
;;; https://github.com/emacs-eclim/emacs-eclim
;;; http://www.skybert.net/emacs/java/

;;; Depends on external tools:
;;; eclipse

;; emacs-eclim fails to load with use-package for some reason
(install-if-missing '(flycheck
                      emacs-eclim
                      company))

(custom-set-variables
 '(eclim-eclipse-dirs '("/local/sram/eclipse"))
 '(eclim-executable "/local/sram/eclipse/eclim"))

(require 'eclim)
(global-eclim-mode)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;; manual completion trigger should use company mode as well
(define-key eclim-mode-map (kbd "C-M-i") 'company-emacs-eclim)

(provide 'my-java)
;;; my-java ends here
