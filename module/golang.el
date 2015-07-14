(provide 'golang)

(require 'common)

; We assume that $GOROOT and $GOPATH env. variables are set and are on the $PATH.
; Also it is mandatory to have them on path in order for the CLI tools like godef that are installed using 'go get' work.
; References:
; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/ 
; http://dominik.honnef.co/posts/2013/08/writing_go_in_emacs__cont__/
; http://rz.scale-it.pl/2013/03/04/emacs_on_fly_syntax_checking_for_go_programming_language.html

(defvar golang-package `(go-mode flycheck go-autocomplete))

(install-if-missing golang-package)

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(ac-config-default)
