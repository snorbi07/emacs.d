;;; my-adnovum --- module provides support for working in AdNovum's corporate environment
;;; Commentary:

;;; Code:

(setq url-proxy-services '(("https" . "proxy.adnovum.hu:3128")
			     ("http" . "proxy.adnovum.hu:3128")))

(defun my-adn-vm-shell ()
  "Opens a shell to my personal AdNovum corporate virtual machine."
  (interactive)
  (let ((default-directory "/sram@adnvlhu003:"))
    (shell)))

;; use git-bash on Windows
(setq explicit-shell-file-name
      "C:/Program Files/Git/bin/bash.exe")
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:/Program Files/Git/bin")
(setq explicit-bash.exe-args '("--login" "-i"))


(provide 'my-adnovum)
;;; my-adnovum.el ends here
