(setq url-proxy-services '(("https" . "proxy.adnovum.hu:3128")
			     ("http" . "proxy.adnovum.hu:3128")))

(defun adn-vm-shell ()
  (interactive)
  (let ((default-directory "/sram@adnvlhu003:"))
    (shell)))

(provide 'my-adnovum)
