;;; neotree.el -*- lexical-binding: t; -*-

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree" "d n" #'neotree-dir)
