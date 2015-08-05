;;UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat t)
(scroll-bar-mode -1)

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


;;Pddsadackage management
;configure the path for local packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/core"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/module"))

;load common function definitions, since those are used everywhere
(require 'common)

;use additional repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;define the default packages that should be installed
(defvar my-packages '(
			;paredit
			ido-ubiquitous
			;find-file-in-project
			;magit
			smex
			;scpaste
			))

(install-if-missing my-packages)

;;Load various configuration extensions

; use ido by default and wherever possible
(ido-mode 1)
(ido-everywhere 1)

; use ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

; Smex setup
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(global-set-key (kbd "M-x") 'smex)

; are we in "the corporate" environment, if yes then load the module
(cond
 ((string-equal system-name "ADNLT098")
  (require 'adnovum)))

; are we running under windows, if yes we need some additional customizations
(cond
 ((string-equal system-type "windows-nt")
  (require 'windows)))

;; Various language specific modules
(require 'golang)
