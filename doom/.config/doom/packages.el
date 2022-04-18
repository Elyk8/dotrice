;; -*- no-byte-compile: t; -*-

(disable-packages!
 irony
 rtags
 evil-snipe
 mpc-mode
 solaire-mode)

(package! dired-open)
(package! dired-subtree)

(package! exwm)

(package! edraw-org :recipe (:host github :repo "misohena/el-easydraw"))

(package! kbd-mode
  :recipe (:host github
           :repo "kmonad/kbd-mode"))

(package! ink :recipe (:host github :repo "foxfriday/ink"))

(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))

(package! org-download)

;; When using org-roam via the `+roam` flag
(unpin! org-roam)

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")) :pin "c93f6b61a8d3d2edcf07eda6220278c418d1054b")
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-auto-tangle)

(package! org-super-agenda)

(package! rotate :pin "4e9ac3ff800880bd9b705794ef0f7c99d72900a6")
