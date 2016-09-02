;;; my-misc.el --- small, general customizations
;;; Commentary:
;;; Add small customizations here, these mostly affect general look and feel or some small behaviour of Emacs.

;;; Code:


; UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat t)
(scroll-bar-mode -1)

;; Default fonts
(custom-set-faces
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight semi-bold :height 98 :width normal)))))

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

;; General key bindings
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key (kbd "M-o") 'other-window)


(provide 'my-misc)
;;; my-misc.el ends here
