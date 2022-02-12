;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; MAPPINGS
(map! :nv ";" 'evil-ex
      :nv ":" 'evil-snipe-repeat
      :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      :prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines)

;; MODELINE
(set-face-attribute 'mode-line nil :font "Ubuntu Mono-12")
(setq doom-modeline-height 20     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

;; MOUSE
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

;; SETTINGS
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 display-line-numbers-type 'relative)                     ; Display line numbers

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      ;; auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin
(global-subword-mode 1)                           ; Iterate through CamelCase words

;; EVIL
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

;; FONTS AND THEMES
;; Themes
(setq doom-theme 'doom-dark+)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

;; Fonts
(setq doom-font (font-spec :family "Ubuntu Nerd Font" :size 22)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 22)
      ;; doom-unicode-font (font-spec :family "JuliaMono")
      doom-big-font (font-spec :family "Ubuntu Nerd Font" :size 32))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(delq! t custom-theme-load-path)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; WINDOWS
(setq split-height-threshold 0)
(setq split-width-threshold 0)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

;; WINDOW TITLE
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" " ● %s") project-name))))))

;; WHICH KEY
(setq which-key-idle-delay 0.1) ;; I need the help, I really do
;; Sometimes, `evil-`⎘is too verbose
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "modules/dashboard.el")
(load! "modules/emacseverywhere.el")
(load! "modules/ibuffer.el")
(load! "modules/neotree.el")
(load! "modules/org.el")
(load! "modules/orgroam.el")
(load! "modules/dired.el")
