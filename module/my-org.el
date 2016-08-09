;;; my-org.el --- org mode or related configurations and extensions
;;; Commentary:
;;; Add reusable functions to this file

;;; Code:

(use-package org
  :ensure t
  :bind (("C-c o a" . org-agenda))
  :config (setq org-agenda-files (list "~/Documents/org")))

(provide 'my-org)
;;; my-org.el ends here
