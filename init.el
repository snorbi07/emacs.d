;;UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat t)


;;Behaviour
;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
;disable splash screen stuff
(setq inhibit-startup-screen t)


;;Key bindings
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key (kbd "M-o") 'other-window)


;;Package management
;configure the path for local packages
(add-to-list 'load-path "~/emacs.d/")

;load common function definitions, since those are used everywhere
(require 'common)

;use additional repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;define the default packages that should be installed
(defvar my-packages '(
			;better-defaults
			;paredit
			;idle-highlight-mode
			;ido-ubiquitous
			;find-file-in-project
			;magit
			;smex
			;scpaste
			))

(install-if-missing my-packages)

;;Load various configuration extensions





