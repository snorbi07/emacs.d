(provide 'common)

(defun install-if-missing (packages)
  "Install the given list of packages if missing."
  (package-initialize)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))
