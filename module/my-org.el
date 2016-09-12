;;; my-org.el --- org mode or related configurations and extensions
;;; Commentary:
;;; Add reusable functions to this file

;;; Code:

(use-package org
  :ensure t
  :bind (("C-c o a" . org-agenda))
  :config (setq org-agenda-files (list "~/Documents/org/")
		org-default-notes-file "~/Documents/org/adhoc.org"
		org-capture-templates
		(quote (("t" "Todo" entry (file "~/Documents/org/adhoc.org") "* TODO %^{Todo description} %? Added: %U %i %a")
			("d" "Scheduled task" entry (file "~/Documents/org/adhoc.org") "* TODO %^{Task description} %? \nSCHEDULED: %t %i %a")))))

(provide 'my-org)
;;; my-org.el ends here
