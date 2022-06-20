;; -*- no-byte-compile: t; -*-

(disable-packages!
 evil-escape
 irony
 rtags
 solaire-mode)

(unpin!
 org
 org-mode)

(package! dired-sidebar)

(package! org-super-agenda)
(package! org-appear)
(package! org-auto-tangle)
(package! org-modern)
(package! org-modern-indent :recipe (:host github :repo "jdtsmith/org-modern-indent"))

(package! org-roam)
(package! websocket)
(package! org-roam-bibtex)
(package! org-roam-ui)

(package! org-roam-bibtex)

(package! kbd-mode
  :recipe (:host github
           :repo "kmonad/kbd-mode"))
(package! centered-cursor-mode)
(package! platformio-mode)
