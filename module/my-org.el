;;; my-org.el --- org mode or related configurations and extensions
;;; Commentary:
;;; Add reusable functions to this file

;;; Code:

(defvar my-org-folder-path "~/Documents/org")

(when (string-equal system-type "windows-nt") (setq my-org-folder-path "~/../../Dropbox/org"))

(defvar my-org-notes-file-path (concat my-org-folder-path "/gtd/refile.org"))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c o a" . org-agenda)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	 ("C-c C-r" . org-refile))
  :config (progn
	    (setq org-agenda-files (list (concat my-org-folder-path "/gtd"))
		  org-refile-use-outline-path t
		  org-refile-targets  '((org-agenda-files :maxlevel . 3))
		  org-log-into-drawer t ; task related notes should end up in the default LOGBOOK drawer
		  org-clock-into-drawer "CLOCKING" ; by default clocking information would end up in the LOGBOOK as well
		  org-default-notes-file my-org-notes-file-path
		  org-capture-templates
		  (quote (("t" "TODO" entry (file+headline my-org-notes-file-path "Captures") "* TODO %^{Todo description}")
			  ("T" "TODO" entry (file+headline my-org-notes-file-path "Captures") "* TODO %^{Todo description}\n %? Added: %U %i %a"))))
	    (use-package org-bullets
	      :ensure t
	      :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))))

(provide 'my-org)
;;; my-org.el ends here
