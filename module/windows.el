(provide 'windows)

; ispell alternative for windows. Requires a working aspell installation.
; for details, check: http://emacswiki.org/emacs/AspellWindows
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
(setq ispell-program-name "aspell")
(require 'ispell)
  
; disable that annoying windows beep
(setq visible-bell 1)
