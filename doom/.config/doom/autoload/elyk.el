;;; ~/.config/doom/autoload/elyk.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/dotrice via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotrice")))

(defun find-in-configs ()
  "Open a file somewhere in ~/.config via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.config/")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/dotrice."
  (interactive)
  (doom-project-browse (expand-file-name "~/.dotrice/")))

;;;###autoload
(defun find-in-scripts ()
  "Open a file somewhere in scripts directory, ~/script via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.scripts")))

;;;###autoload
(defun find-in-suckless ()
  "Open a file somewhere in the suckless directory, ~/.local/src via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.local/src/")))

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
(defun elk/exwm-update-global-keys ()
  "Function to apply changes to `exwm-input-global-keys'"
  (interactive)
  (setq exwm-input--global-keys nil)
  (dolist (i exwm-input-global-keys)
    (exwm-input--set-key (car i) (cdr i)))
  (when exwm--connection
    (exwm-input--update-global-prefix-keys)))
