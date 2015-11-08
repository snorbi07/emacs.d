;;; my-clojure --- module provides support for working with Clojure
;;; Commentary:

;;; A working installation of Cljoure is assumed

;;; Code:


(require 'my-common)

(defvar clojure-packages `(
			   projectile
			   clojure-mode
			   cider))

(install-if-missing clojure-packages)

(provide 'my-clojure)
;;; my-clojure.el ends here

