;; Init.el --- entry point for my custom emacs configuration
;;; Commentary:
;;; The init.el file is made up of 4 sections:
;;; Initializations - set up load paths, remote repository paths and load environment specific configurations.
;;; Customizations - Look and feel plus key bindings.
;;; Packages - Load and configure 3rd party packages from Emacs repositories.
;;; Modules - Load personal modules, these usually depend on 3rd party libraries.


;;; Code:
;;; Initialization:

;; Package management --- configure the repository and bootstrap use-package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
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


(use-package my-misc
  :load-path "module/")


(use-package smart-mode-line
  :ensure t
  :config (progn
	    ;; to avoid the 'loading the theme can run Lisp code' message/question
	    (setq custom-safe-themes t)
	    (sml/setup)
	    (setq sml/theme 'dark)))


(use-package avy
  :ensure t)


(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))


(use-package key-chord
  :ensure t
  :after avy
  :config
  (progn
    (setq key-chord-one-key-delay 0.17)
    (key-chord-mode 1)
    (key-chord-define-global "jj"     'avy-goto-char)
    (key-chord-define-global "jw"     'avy-goto-word-1)
    (key-chord-define-global "jl"     'avy-goto-line)))


(use-package paredit
  :ensure t
  :config (progn
	    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
	    ;; for the rest of the modes smartparens is used
	    ;;(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
	    ;;(add-hook 'lisp-mode-hook             'enable-paredit-mode)
	    ;;(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
	    ;;(add-hook 'scheme-mode-hook           'enable-paredit-mode)
	    ))


(use-package yaml-mode
  :defer t
  :ensure t)


(use-package helm
  :ensure t
  :defer t
  :config
  (progn
    (require 'helm-config)
    (helm-adaptive-mode 1)
    (helm-push-mark-mode 1)
    (helm-autoresize-mode 1)
    (helm-adaptive-mode 1)
    (helm-mode 1)
    ;; helm-descbinds configuration
    ;; Currently not bound to any key so just run the helm-descbinds function
    ;; C-z provides a description of the command
    (use-package helm-descbinds
      :ensure t
      :config (helm-descbinds-mode))

    ;; used by projectile
    (use-package helm-ag
      :ensure t
      :config (progn
		(use-package ag
		  :ensure t)))

    ;; helm extension to projectile. use-package makes sure it is installed,
    ;; by default the configuration is done in the projectile use-package declaration
    (use-package helm-projectile
      :ensure t
      :after helm
      ;; Workaround for issue: https://github.com/bbatsov/helm-projectile/issues/30
      :bind
      (:map projectile-command-map
	    ("b" . helm-projectile-switch-to-buffer)
	    ("d" . helm-projectile-find-dir)
	    ("f" . helm-projectile-find-file)
	    ("p" . helm-projectile-switch-project)
	    ("s s" . helm-projectile-ag))
      :config (helm-projectile-toggle 1)))

  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-c f" . helm-recentf)
   ("C-x C-f" . helm-find-files)
   ("C-c SPC" . helm-all-mark-rings)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-h r" . helm-info-emacs)
   ("C-," . helm-calcul-expression)
   ("C-h i" . helm-info-at-point)
   ("C-x C-d" . helm-browse-project)
   ("C-h C-f" . helm-apropos)
   ("C-c i" . helm-imenu-in-all-buffers)
   ("C-s" . helm-occur)
   ("M-g a" . helm-do-grep-ag)
   ([remap jump-to-register] . helm-register)
   ([remap list-buffers] . helm-buffers-list)
   ([remap dabbrev-expand] . helm-dabbrev)
   ([remap find-tag] . helm-etags-select)
   ([remap xref-find-definitions] . helm-etags-select)))


(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))


(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-strict-mode t)
    (sp-use-smartparens-bindings)
    (smartparens-global-mode t)))


;; emacs-which-key
;; Pop up to display possible follow up commands
(use-package which-key
  :ensure t
  :config (which-key-mode))


;; projectile used for development tasks
(use-package projectile
  :ensure t
  :defer t
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t
	  projectile-completion-system 'helm)
    (projectile-global-mode)
    (helm-projectile-on)
    
    (use-package neotree
      :ensure t
      :config
      (setq projectile-switch-project-action 'neotree-projectile-action
	    neo-smart-open t)
      :bind
      ([f8] . neotree-toggle))))


(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))


(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook
	      (lambda()
		(highlight-symbol-mode 1)))
    (add-hook 'prog-mode-hook
	      (lambda()
		(highlight-symbol-nav-mode 1)))

    (setq highlight-symbol-idle-delay 1.0
	  highlight-symbol-on-navigation-p t))
  :diminish highlight-symbol-mode)


(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'global-company-mode)
    (use-package company-quickhelp
      :ensure t
      :config (company-quickhelp-mode t))))


;; enable spell checking during coding as well, since I cannot spell properly anyways.
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;; My Modules:

;; org and related configurations
(use-package  my-org
  :load-path "module/")

;;; init.el ends here
