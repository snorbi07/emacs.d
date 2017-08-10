;;; my-org.el --- org mode or related configurations and extensions
;;; Commentary:
;;; Add reusable functions to this file

;;; Code:

(defvar my-org-folder-path "~/Documents/org")

(when (string-equal system-type "windows-nt") (setq my-org-folder-path "~/../../Dropbox/org"))

(defvar my-org-notes-file-path (concat my-org-folder-path "/notes.org"))

(use-package org
  :ensure t
  :bind (("C-c o a" . org-agenda))
  :config (progn
	    (setq org-agenda-files (list my-org-folder-path)
		  org-default-notes-file my-org-notes-file-path
		  org-capture-templates
		  (quote (("t" "Todo" entry (file my-org-notes-file-path) "* TODO %^{Todo description} %? Added: %U %i %a")
			  ("d" "Scheduled task" entry (file my-org-notes-file-path) "* TODO %^{Task description} %? \nSCHEDULED: %t %i %a"))))
	    (use-package org-bullets
	      :ensure t
	      :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))))

(provide 'my-org)
;;; my-org.el ends here
