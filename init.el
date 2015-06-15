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


;;Package management
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;;Packages that I use
(defvar sram-packages '(
			;better-defaults
			;paredit
			;idle-highlight-mode
			;ido-ubiquitous
			;find-file-in-project
			;magit
			;smex
			;scpaste
			))

;;Install packages that I use if missing
(package-initialize)
(dolist (p sram-packages)
  (when (not (package-installed-p p))
    (package-install p)))

