;;; init.el --- entry point for my custom emacs configuration
;;; Commentary:
;;; The init.el file is made up of 4 sections:
;;; Initializations - set up load paths, remote repository paths and load environment specific configurations.
;;; Customizations - Look and feel plus key bindings.
;;; Packages - Load and configure 3rd party packages from Emacs repositories
;;; Modules - Load personal modules. These usually depend on 3rd party libraries.


;;; Code:
;;; Initialization:

;; Package management --- configure the repository and bootstrap use-package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Pre-load (env) section --- scripts that need to be run before starting with the setup/customization. Think of plantform/environment specifict settings, such as proxy configuration.
;; are we in "the corporate" environment, if yes then load the module

;; FIXME: currently this requires manual bootstraping if a proxy configuration is needed to install use-package from melpa
(use-package my-adnovum
  :load-path "env/"
  :if (string-equal system-name "ADNLT098"))

;; are we running under windows, if yes we need some additional customizations
(use-package  my-windows
  :load-path "env/"
  :if (string-equal system-type "windows-nt"))



;;; Customizations:

;; UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat t)
(scroll-bar-mode -1)

;; Default fonts
(custom-set-faces
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 102 :width normal)))))

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
		      yaml-mode
		      expand-region
		      helm
		      smart-mode-line
                      smartparens
		      ;; Currently unused, howere there are commented customizations for these packages:
		      ;ido-ubiquitous
		      ;smex
		      ))

;; FIXME: this can be removed after the refactoring to use-package has been completed
(use-package my-common
  :load-path "core/")
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


;; Smart-mode-line configuration
; to avoid the 'loading the theme can run Lisp code' message/question
(setq custom-safe-themes t)
(sml/setup)
(setq sml/theme 'dark)


;; smartparens configuration
(require 'smartparens-config)


;; helm-descbinds configuration
;; Currently not bound to any key so just run the helm-descbinds function
;; C-z provides a description of the command
(use-package helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))
  


;;; Modules:

;; Various language specific modules
;; (require 'golang)
(use-package my-javascript
  :load-path "module/")
(use-package my-elixir
  :load-path "module/")
;;; init.el ends here
