;;; my-common.el --- custom generic functions
;;; Commentary:
;;; Add reusable functions to this file

;;; Code:

(defun install-if-missing (packages)
  "Install the given list of PACKAGES if missing."
  (package-initialize)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'my-common)
;;; my-common.el ends here
