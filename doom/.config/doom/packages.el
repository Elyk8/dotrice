;; [[file:config.org::*Packages][Packages:11]]
;; -*- no-byte-compile: t; -*-

;; (package! dashboard)
(package! dired-open)
(package! dired-subtree)
(package! elfeed-goodies)
(package! emojify)
(package! org-fragtog)

(package! org-appear)

;; org-roam-ui tries to pull latest changes from org-roam, so we unpin it
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")) :pin "c93f6b61a8d3d2edcf07eda6220278c418d1054b")
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'

;; Graphs packages
(package! ink :recipe (:host github :repo "foxfriday/ink"))

(disable-packages!
 mpc-mode
 solaire-mode)
;; Packages:11 ends here
