;;; my-adnovul --- module provides support for working in AdNovum's corporate environment
;;; Commentary:

;;; Code:

(setq url-proxy-services '(("https" . "proxy.adnovum.hu:3128")
			     ("http" . "proxy.adnovum.hu:3128")))

(defun my-adn-vm-shell ()
  "Opens a shell to my personal AdNovum corporate virtual machine."
  (interactive)
  (let ((default-directory "/sram@adnvlhu003:"))
    (shell)))

(provide 'my-adnovum)
;;; my-adnovum.el ends here
