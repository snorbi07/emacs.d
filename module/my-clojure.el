(require 'common)

(defvar clojure-packages `(
			   projectile
			   clojure-mode
			   cider))

(install-if-missing clojure-packages)

(provide 'my-clojure)

