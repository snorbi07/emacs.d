(provide 'golang)

(require 'common)

; We assume that $GOROOT and $GOPATH env. variables are set and are on the $PATH.
; Also it is mandatory to have them on path in order for the CLI tools like godef that are installed using 'go get' work.

; External/Tool dependencies:
;                                 
; go get -u github.com/nsf/gocode
; go get code.google.com/p/rog-go/exp/cmd/godef
; go get -u github.com/dougm/goflymake
; 


; References:
; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/ 
; http://dominik.honnef.co/posts/2013/08/writing_go_in_emacs__cont__/
; http://rz.scale-it.pl/2013/03/04/emacs_on_fly_syntax_checking_for_go_programming_language.htmln
; http://yousefourabi.com/blog/2014/05/emacs-for-go/


(defvar golang-package `(go-mode flycheck company company-go))

(install-if-missing golang-package)

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'company-mode)

; This disables other backends of the company-mode. Potentially screws up other modes! In the long run it might be a better idea to remove this.
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))