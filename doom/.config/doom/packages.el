;; [[file:config.org::*My Packages][My Packages:1]]
;; -*- no-byte-compile: t; -*-

;; (package! dashboard)
;; (package! frames-only-mode :recipe (:host github :repo "davidshepherd7/frames-only-mode"))
(package! dired-open)
(package! dired-subtree)
(package! elfeed-goodies)
(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "148aa124901ae598f69320e3dcada6325cdc2cf0")

(package! emacs-reveal :recipe (:host gitlab :repo "oer/emacs-reveal"))
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")) :pin "c93f6b61a8d3d2edcf07eda6220278c418d1054b")
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'

(package! org-super-agenda)
(package! kbd-mode
  :recipe (:host github
           :repo "kmonad/kbd-mode"))

;; Graphs packages
(package! ink :recipe (:host github :repo "foxfriday/ink"))

(disable-packages!
 irony
 rtags
 evil-snipe
 mpc-mode
 solaire-mode)
;; My Packages:1 ends here
