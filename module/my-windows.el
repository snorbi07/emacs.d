; ispell alternative for windows. Requires a working aspell installation.
; for details, check: http://emacswiki.org/emacs/AspellWindows
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
(setq ispell-program-name "aspell")
(require 'ispell)
  
; disable that annoying windows beep
(setq visible-bell 1)

; load tramp and configure it to work with Putty (assuming that Putty is on the PATH)
(require 'tramp)
(set-default 'tramp-auto-save-directory "C:/Users/sram/AppData/Local/Temp")
(set-default 'tramp-default-method "plink")

; this assumes a working cygwin installation on the PATH
(setq shell-file-name "bash")

(provide 'my-windows)

