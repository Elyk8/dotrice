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
  (doom-project-find-file (expand-file-name "~/.local/src/")))

;;;###autoload
(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (mu4e~start)))

;;;###autoload
(defun org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (s-matches-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
      (message "Replaced %d occurances" count))))

;;;###autoload
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

;;;###autoload
(defun elk/org-roam-rename-to-new-title ()
  "Change the file name after changing the title."
  (when-let*
      ((old-file (buffer-file-name))
       (is-roam-file (org-roam-file-p old-file))
       (is-roam-buffer (org-roam-buffer-p))
       (file-node (save-excursion
                    (goto-char 1)
                    (org-roam-node-at-point)))
       (slug (org-roam-node-slug file-node))
       (new-file (expand-file-name (replace-regexp-in-string "-.*\\.org" (format "-%s.org" slug) old-file)))
       (different-name? (not (string-equal old-file new-file))))
    (rename-buffer (file-name-nondirectory new-file))
    (rename-file old-file new-file 1)
    (set-visited-file-name new-file)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun crontab-e ()
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))
