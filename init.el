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


;; Define a new prefix key, which is used by my own shortcuts, otherwise we cannot bind a command to M-m M-m for example.
(define-prefix-command 'my-keymap)
(global-set-key (kbd "M-m") 'my-keymap)

;; Change the annoying behavior of "C-z"/suspend...
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'my-suspend-frame)
(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Pre-load (env) section --- scripts that need to be run before starting with the setup/customization. Think of plantform/environment specifict settings, such as proxy configuration.
;; are we in "the corporate" environment, if yes then load the module

;; are we running under windows, if yes we need some additional customizations
(use-package  my-windows
  :load-path "env/"
  :if (string-equal system-type "windows-nt"))



;; UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-safe-themes t)

(use-package idea-darkula-theme
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(use-package material-theme
  :ensure t
  :init (load-theme 'material))

;; Default font setting
(add-to-list 'default-frame-alist
	     '(font . "Hack-10"))

;; Behavior
;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; disable splash screen stuff
(setq inhibit-startup-screen t)
;; change all promts to 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)
;; disable annoying "beep" sound
(setq visible-bell t)
;; display the current time
(display-time-mode)
;; don't start in prog-mode by default, this way we achieve a faster startup time because prog-mode has various hooks associated with it.
(setq initial-major-mode 'text-mode)
;; disable window narrowing/widening message
(put 'narrow-to-region 'disabled nil)
;; TRAMP
;; make the default method ssh instead of scp
(set-default 'tramp-default-method "ssh")


;; Dired
;; to get the midnight commander experience!
(setq dired-dwim-target t)

;; General key bindings
(global-set-key [f11] 'toggle-frame-fullscreen)
;; disable suspend-frame bindings
(global-set-key "\C-x\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)


(use-package crux
  :ensure t
  :bind (("S-<return>" . crux-smart-open-line)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-<backspace>" . crux-kill-line-backwards)))


(use-package smart-mode-line
  :ensure t
  :config (progn
	    ;; to avoid the 'loading the theme can run Lisp code' message/question
	    (setq custom-safe-themes t)
	    (sml/setup)
	    (setq sml/theme 'dark)))

(use-package avy
  :ensure t
  :bind (("M-m M-m" . 'avy-goto-word-1)
	 ("M-m M-c" . 'avy-goto-char)
	 ("M-m M-l" . 'avy-goto-line)))


(use-package imenu-anywhere
  :ensure t
  :bind (("M-m m" . 'helm-imenu)))


(use-package eyebrowse
  :ensure t
  :config (eyebrowse-mode t))


(use-package winner
  :config (winner-mode t))


(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))


(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))


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
  :config
  (progn
    (require 'helm-config)
    (helm-adaptive-mode 1)
    (helm-autoresize-mode 1)
    (helm-adaptive-mode 1)
    (helm-mode 1)
    ;; helm-descbinds configuration
    ;; Currently not bound to any key so just run the helm-descbinds function
    ;; C-z provides a description of the command
    (use-package helm-descbinds
      :ensure t
      :config (helm-descbinds-mode))

    (use-package helm-swoop
      :ensure t
      ;https://emacs.stackexchange.com/questions/28089/how-do-i-make-helm-swoop-not-default-the-search-to-the-word-at-point
      :config (setq helm-swoop-pre-input-function (lambda () ""))
      :bind (("C-s" . helm-swoop)))

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
   ("M-g a" . helm-do-grep-ag)
   ([remap switch-to-buffer] . helm-buffers-list)
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
    (sp-use-smartparens-bindings)
    (smartparens-global-mode t)
    (setq sp-highlight-pair-overlay nil)
    (show-smartparens-global-mode t)
    ;; Smart auto-line after a paratnheses pair: https://stackoverflow.com/questions/22107501/set-emacs-to-smart-auto-line-after-a-parentheses-pair
    (sp-with-modes '(c-mode c++-mode typescript-mode javascript-mode js-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))))


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
    (helm-projectile-on)))


(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))


(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-mode . lsp)
	 (js2-mode . lsp)
         ;; for which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


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


(use-package adoc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))


(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))


(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'global-company-mode)
    ; enable the C- based navigation in tha auto-complente popup
    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    (setq company-tooltip-align-annotations t
	  company-idle-delay 0
	  company-quickhelp-delay 0
	  company-dabbrev-downcase nil)
    (use-package company-quickhelp
      :ensure t
      :config (company-quickhelp-mode t))))


;; Used for debugging purposes
(use-package realgud
  :ensure t)


(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))


;; Python development setup
;; Don't forget to install the needed external dependencies: https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :init (progn (elpy-enable)
	       (add-hook 'python-mode-hook #'subword-mode)))

;; Check that we have we have 'pdb' on PATH, since realgud tries to call that directly
;; If it doesn't exist, try creating a bash script for it and adding it to the PATH:
;;
;; #!/bin/sh
;; exec python -m pdb "$@"
;;
(unless (executable-find "pdb") (message "'pdb' not found on $PATH, Python debugging won't work!"))


;; Anaconda based Python support, disabled because of elpy usage
;(use-package anaconda-mode
;  :ensure t
;  :init
;  (progn
;    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;    (add-hook 'python-mode-hook 'anaconda-mode))
;  :config
;  (progn
;    (use-package flycheck-pyflakes
;      :ensure t)
;    (use-package company-anaconda
;      :ensure t
;      :init (add-to-list 'company-backends '(company-anaconda :with company-capf)))
;    (use-package pyenv-mode
;      :ensure t
;      :init (add-hook 'python-mode-hook 'pyenv-mode)
;      :config
;      (progn
;	(defun projectile-pyenv-mode-set ()
;	  "Set pyenv version matching project name."
;	  (let ((project (projectile-project-name)))
;	    (if (member project (pyenv-mode-versions))
;		(pyenv-mode-set project)
;	      (pyenv-mode-unset))))
;	(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
;        ; disable the global mapping introduced by pyenv, since it conflicts with other modes such as org-agenda-scheduel
;	(define-key pyenv-mode-map (kbd "C-c C-s") nil)))))


(use-package elixir-mode
  :ensure t
  :defer t
  :commands elixir-mode
  :init
  (progn
    (add-hook 'elixir-mode-hook #'flycheck-mode)
    (add-hook 'elixir-mode-hook #'subword-mode)
    (add-hook 'elixir-mode-hook #'alchemist-mode-hook))
  :config
  (progn
    (use-package alchemist
      :ensure t)
    ;;FIXME: seems to have an issue with buffer path, similarly to:
    ;; https://github.com/mpenet/clojure-snippets/pull/7
    ;; https://github.com/syl20bnr/spacemacs/issues/5373
    ;;  (use-package elixir-yasnippets
    ;;  :ensure t)
    ;; To reproduce: after starting up emacs create an elixir buffer, an error will be displayed. If another prog-mode one is triggered beforehand it works.
    (sp-with-modes '(elixir-mode)
      (sp-local-pair "fn" "end"
    		     :when '(("SPC" "RET"))
    		     :actions '(insert navigate))
      (sp-local-pair "do" "end"
    		     :when '(("SPC" "RET"))
    		     :post-handlers '(sp-ruby-def-post-handler)
    		     :actions '(insert navigate)))
    (use-package flycheck-mix
      :ensure t
      :init (add-hook 'elixir-mode-hook 'flycheck-mix-setup))))


(use-package groovy-mode
  :ensure t
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))


(use-package typescript-mode
  :ensure t
  :defer t
  :config (progn (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))))

;; JavaScript support and related configuration
(use-package js2-mode
  :ensure t
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
  :ensure t
  :defer t
  :config
  (progn
    (use-package tern
      :ensure t)
    (use-package company-tern
      :ensure t
      :config (add-to-list 'company-backends 'company-tern))
    ;; Turn off js2 mode errors & warnings (use eslint/standard/... instead through flycheck)
    ;;(setq js2-mode-show-parse-errors nil
    ;;      js2-mode-show-strict-warnings nil)
    (add-hook 'rjsx-mode-hook #'tern-mode)
    (add-hook 'rjsx-mode-hook #'company-mode))
  :init (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))


;; JSON mode configuration
;; This here, seperatelly from JS/TS since it is possible to open a JSON file without triggering the other 2 modes.
(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'"
  :config (setq js-indent-level 2))


;; Support for various template formats
(use-package web-mode
  :ensure t
  :defer t
  :config
  (progn
    ;; adjust indents for web-mode to 2 spaces
    (defun my-web-mode-hook ()
      "Hooks for Web mode.  Adjust indent."
      (setq web-mode-markup-indent-offset 2
	    web-mode-css-indent-offset 2
	    web-mode-code-indent-offset 2))
    (add-hook 'web-mode-hook  'my-web-mode-hook)
    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; add hooks for everything!
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))
  )


(use-package sbt-mode
  :ensure t
  :defer t
  :pin melpa)

(use-package scala-mode
  :ensure t
  :defer t
  :pin melpa)


(use-package protobuf-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode)))


(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
	  ;; set to nil, otherwise does not work properly with my current setup
          treemacs-display-in-side-window        nil
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.1
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         t
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("M-m t t"   . treemacs)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; needed to ensure that swoop/helm works with treemacs... sort of.
(use-package shackle
  :ensure t
  :config (setq shackle-default-rule '(:same t)))


(use-package editorconfig
  :ensure t
  :config (progn
	    (unless (executable-find "editorconfig") (message "'editorconfig' not found on $PATH, editorconfig won't work!"))
	    (editorconfig-mode 1)))


;; enable spell checking during coding as well, since I cannot spell properly anyways.
;; FIXME: disabled since it conflicts with C-M-i binding of company by default
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;; My Modules:

;; org and related configurations
(use-package  my-org
  :load-path "module/")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(material-theme company-tern tern org-bullets editorconfig shackle treemacs-magit treemacs-icons-dired treemacs-projectile treemacs protobuf-mode scala-mode sbt-mode ensime web-mode rjsx-mode js2-mode typescript-mode groovy-mode elixir-mode elpy yasnippet realgud company-quickhelp company flycheck adoc-mode highlight-symbol company-lsp lsp-ui lsp-mode magit which-key smartparens expand-region helm-projectile ag helm-ag helm-swoop helm-descbinds helm yaml-mode paredit ace-window undo-tree eyebrowse imenu-anywhere avy smart-mode-line crux zenburn-theme idea-darkula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
