;; -*- no-byte-compile: t; -*-
;;; wmintegration/bspwm/packages.el

(package! emacs-bspwm-integration
  :recipe (:host github :repo "lcvdkmp/emacs-bspwm-integration" :build (:not compile)))
