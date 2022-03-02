;;; ~/.config/doom/autoload/elyk.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/dotrice via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/dotrice")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/dotrice."
  (interactive)
  (doom-project-browse (expand-file-name "~/dotrice")))
