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
