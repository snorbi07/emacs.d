;;; my-org.el --- org mode or related configurations and extensions
;;; Commentary:
;;; Add reusable functions to this file

;;; Code:
(require 'org)
(require 'org-capture)

; set up capture support for taking random notes effortlessly
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

(provide 'my-org)
;;; my-org.el ends here
