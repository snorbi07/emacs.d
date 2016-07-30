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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

;; Behaviour
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


;; General key bindings
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key (kbd "M-o") 'other-window)


(provide 'my-misc)
;;; my-misc.el ends here
