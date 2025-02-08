;; Agenda variables
(setq org-directory "~/Documents/org/gtd/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "ggdev.org" "personal.org"))

;; Default tags
;(setq org-tag-alist '(
;                      ;; locale
;                      (:startgroup)
;                      ("home" . ?h)
;                      ("work" . ?w)
;                      ("meetup" . ?s)
;                      (:endgroup)
;                      (:newline)
;                      ;; scale
;                      (:startgroup)
;                      ("one-shot" . ?o)
;                      ("project" . ?j)
;                      ("tiny" . ?t)
;                      (:endgroup)
;                      ;; misc
;                      ("meta")
;                      ("review")
;                      ("reading")))
;
;; Org-refile: where should org-refile look?
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!
  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
	      ("C-c o a" . org-agenda)
	      ("C-c c" . org-capture)
	      ("C-c C-r" . org-refile)
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "WAITING(w@/!)" "PROJECT(p)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c!)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
	  ;; Define Personal section
          ("p" "Personal")
	  ("pe" "Errands" entry (file+headline "personal.org" "PROJECT Errands")
           "** TODO %?\n%U\n%i\n%a")
          ("ph" "Homelab" entry (file+headline "personal.org" "PROJECT Homelab")
           "** TODO %?\n%U\n%i\n%a")
	  ("ps" "Sharpening the saw" entry (file+headline "personal.org" "PROJECT Sharpening the saw")
           "** TODO %?\n%U\n%i\n%a")
	  ;; Define Work section
          ("w" "Work")
          ("wc" "GG Company Mgm." entry (file+headline "ggdev.org" "PROJECT GG Company Management")
           "** TODO %?\n%U\n%i\n%a")
	  ("wd" "DGate" entry (file+headline "ggdev.org" "PROJECT DGate")
           "** TODO %?\n%U\n%i\n%a")
	  ("wi" "IndaPlay" entry (file+headline "ggdev.org" "PROJECT IndaPlay")
           "** TODO %?\n%U\n%i\n%a")
	  ("wm" "Manis" entry (file+headline "ggdev.org" "PROJECT Manis")
           "** TODO %?\n%U\n%i\n%a")
	  ("wa" "Axis" entry (file+headline "ggdev.org" "PROJECT Axis")
           "** TODO %?\n%U\n%i\n%a")
	  ("wg" "GroupM" entry (file+headline "ggdev.org" "PROJECT GroupM")
           "** TODO %?\n%U\n%i\n%a")
	  ("wx" "GG Day-to-day" entry (file+headline "ggdev.org" "PROJECT Day-to-day")
           "** TODO %?\n%U\n%i\n%a")
	  ))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work" agenda ""
           ((org-agenda-files '("work.org")))))))

