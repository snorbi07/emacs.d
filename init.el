;;; init.el --- entry point for my custom emacs configuration
;;; Commentary:

;; TODO - add description


;;; Code:
;;; Initialization:

;; Package management --- configure the path for local packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/core"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/module"))

;; use additional repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;; load common function definitions, since those are used everywhere
;; FIXME(snorbi07): does not follow the my-package naming convention!
(require 'common)

;; Pre-load section --- scripts that need to be run before starting with the setup/customization. Think of plantform/environment specifict settings, such as proxy configuration.
;; are we in "the corporate" environment, if yes then load the module
(cond
 ((string-equal system-name "ADNLT098")
  (require 'my-adnovum)))



;;; Customizations:

;; UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat t)
(scroll-bar-mode -1)

;; Behaviour
;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; disable splash screen stuff
(setq inhibit-startup-screen t)
;; change all promts to 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

;; General key bindings
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key (kbd "M-o") 'other-window)



;;; Packages:

;; define the default packages that should be installed
(defvar my-packages '(
		      avy
		      paredit
		      ;ido-ubiquitous
		      smex
		      yaml-mode
		      expand-region
		      helm
		      ))

(install-if-missing my-packages)

;; Load various configuration extensions


;; Ido specific settings
;; To get started with ido, the package needs to be uncommented in the my-packages list.
;; Use ido by default and wherever possible
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (setq ido-enable-flex-matching t)


;; Helm specific settings
(require 'helm-config)
(helm-mode 1)
(helm-adaptive-mode 1)
(helm-push-mark-mode 1)
(helm-autoresize-mode 1)

(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-c f")                        'helm-recentf)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h i")                        'helm-info-at-point)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
(global-set-key (kbd "C-c g")                        'helm-gid)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-s")                          'helm-occur)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)


;; ibuffer specific settings
;; To get started, uncomment the following lines. By default, helm-buffers-list is used instead.
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (autoload 'ibuffer "ibuffer" "List buffers." t)


;; Smex specific settings
;; To enable it, uncomment the following lines. Bt default, helm-M-x is used instead.
;; (autoload 'smex "smex"
;;   "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
;; your recently and most frequently used commands.")
;; (global-set-key (kbd "M-x") 'smex)


;; Avy configuration
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-g f") 'avy-goto-line)


;; Expande-region configuration
(global-set-key (kbd "C-=") 'er/expand-region)



;;; Modules:

;; are we running under windows, if yes we need some additional customizations
(cond
 ((string-equal system-type "windows-nt")
  (require 'my-windows)))

;; Various language specific modules
;; (require 'golang)
(require 'my-javascript)
(require 'my-elixir)
;;; init.el ends here
