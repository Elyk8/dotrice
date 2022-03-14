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

;;;###autoload
(defun find-in-scripts ()
  "Open a file somewhere in scripts directory, ~/script via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/scripts")))

;;;###autoload
(defun find-in-suckless ()
  "Open a file somewhere in the suckless directory, ~/.local/src via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.local/src")))

;;;###autoload
(defun org-rename-to-new-title ()
  "Change the file name after changing the title."
  (when-let*
      ((old-file (buffer-file-name))
       (is-roam-file (org-roam-file-p old-file))
       (in-roam-base-directory? (string-equal
                                 (expand-file-name org-roam-directory)
                                 (file-name-directory old-file)))
       (file-node (save-excursion
                    (goto-char 1)
                    (org-roam-node-at-point)))
       (slug (org-roam-node-slug file-node))
       ;;        (new-file (expand-file-name (concat slug ".org")))
       (new-file (expand-file-name (replace-regexp-in-string "-.*\\.org" (format "-%s.org" slug) (buffer-file-name))))
       (different-name? (not (string-equal old-file new-file))))
    (rename-buffer new-file)
    (rename-file old-file new-file)
    (set-visited-file-name new-file)
    (set-buffer-modified-p nil)))
