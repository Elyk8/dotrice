;;; config.el --- -*- lexical-binding: t -*-

(setq-default
    delete-by-moving-to-trash t                      ; Delete files to trash
    window-combination-resize t                      ; take new window space from all other windows (not just current)
    x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
    evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
    auto-save-default t                         ; Nobody likes to loose work, I certainly don't
    truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
    password-cache-expiry nil                   ; I can trust my computers ... can't I?
    scroll-preserve-screen-position 'always     ; Don't have `point' jump around
    scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line
(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
    (display-battery-mode 1))                       ; it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words

(use-package dashboard
    :init      ;; tweak dashboard config before loading it
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
    (setq dashboard-startup-banner "~/.config/doom/doom-emacs-dash.png")  ;; use custom image as banner
    (setq dashboard-center-content nil) ;; set to 't' for centered content
    (setq dashboard-items '((recents . 5)
                            (agenda . 5 )
                            (bookmarks . 5)
                            (projects . 5)
                            (registers . 5)))
    :config
    (dashboard-setup-startup-hook)
    (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer "*dashboard*")

(map! :leader
    (:prefix ("d" . "dired")
     :desc "Open dired" "d" #'dired
     :desc "Dired jump to current" "j" #'dired-jump)
    (:after dired
    (:map dired-mode-map
     :desc "Peep-dired image previews" "d p" #'peep-dired
     :desc "Dired view file" "d v" #'dired-view-file)))
(evil-define-key 'normal dired-mode-map
    (kbd "M-RET") 'dired-display-file
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
    (kbd "m") 'dired-mark
    (kbd "t") 'dired-toggle-marks
    (kbd "u") 'dired-unmark
    (kbd "C") 'dired-do-copy
    (kbd "D") 'dired-do-delete
    (kbd "J") 'dired-goto-file
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown
    (kbd "P") 'dired-do-print
    (kbd "R") 'dired-do-rename
    (kbd "T") 'dired-do-touch
    (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
    (kbd "+") 'dired-create-directory
    (kbd "-") 'dired-up-directory
    (kbd "% l") 'dired-downcase
    (kbd "% u") 'dired-upcase
    (kbd "; d") 'epa-dired-do-decrypt
    (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "nsxiv")
                              ("jpg" . "nsxiv")
                              ("png" . "nsxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 22)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 22)
      doom-big-font (font-spec :family "Mononoki Nerd Font" :size 34))
(after! doom-themes
    (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
    '(font-lock-comment-face :slant italic)
    '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-dark+)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(setq display-line-numbers-type t)
(map! :leader
     :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
    (:prefix ("t" . "toggle")
     :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
     :desc "Toggle line highlight in frame" "h" #'hl-line-mode
     :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
     :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(require 'which-key)
(setq which-key-idle-delay 0.1)
