;; [[file:config.org::*Lexical Keybinds][Lexical Keybinds:1]]
;;; config.el -*- lexical-binding: t; -*-
;; Lexical Keybinds:1 ends here

;; [[file:config.org::*Async config tangling][Async config tangling:1]]
(defvar +literate-tangle--proc nil)
(defvar +literate-tangle--proc-start-time nil)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (unless (getenv "__NOTANGLE")
    (let ((default-directory doom-private-dir))
      (when +literate-tangle--proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-tangle--proc #'ignore)
        (kill-process +literate-tangle--proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-tangle--proc-start-time (float-time)
            +literate-tangle--proc
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-confirm-babel-evaluate nil \
      org-inhibit-startup t \
      org-mode-hook nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      vc-handled-backends nil \
      org-startup-folded nil \
      org-startup-indented nil) \
(org-babel-tangle-file \"%s\" \"%s\"))"
                                   +literate-config-file
                                   (expand-file-name (concat doom-module-config-file ".el")))))
      (set-process-sentinel +literate-tangle--proc #'+literate-tangle--sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-tangle--sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (+popup-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))))

(defun +literate-tangle-check-finished ()
  (when (and (process-live-p +literate-tangle--proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))
(add-hook! 'kill-emacs-hook #'+literate-tangle-check-finished)
;; Async config tangling:1 ends here

;; [[file:config.org::*Native Compilation][Native Compilation:1]]
;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)
;; Native Compilation:1 ends here

;; [[file:config.org::*Simple Settings][Simple Settings:1]]
(setq-default
    delete-by-moving-to-trash t                 ; Delete files to trash
    window-combination-resize t                 ; take new window space from all other windows (not just current)
    x-stretch-cursor t)                         ; Stretch cursor to the glyph width
(setq undo-limit 80000000                       ; Raise undo-limit to 80Mb
    evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
    truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
    password-cache-expiry nil                   ; I can trust my computers ... can't I?
    scroll-preserve-screen-position 'always     ; Don't have `point' jump around
    scroll-margin 2)                            ; It's nice to maintain a little margin
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; Prevents some cases of Emacs flickering.
;; Simple Settings:1 ends here

;; [[file:config.org::*Simple Settings][Simple Settings:2]]
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)
;; Simple Settings:2 ends here

;; [[file:config.org::*Babel][Babel:1]]
(add-transient-hook! #'org-babel-execute-src-block
  (require 'ob-async))

(defvar org-babel-auto-async-languages '()
  "Babel languages which should be executed asyncronously by default.")

(defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
  "Eagarly add an :async parameter to the src information, unless it seems problematic.
This only acts o languages in `org-babel-auto-async-languages'.
Not added when either:
+ session is not \"none\"
+ :sync is set"
  :around #'org-babel-get-src-block-info
  (let ((result (funcall orig-fn light datum)))
    (when (and (string= "none" (cdr (assoc :session (caddr result))))
               (member (car result) org-babel-auto-async-languages)
               (not (assoc :async (caddr result))) ; don't duplicate
               (not (assoc :sync (caddr result))))
      (push '(:async) (caddr result)))
    result))
;; Babel:1 ends here

;; [[file:config.org::*Bookmarks][Bookmarks:1]]
(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))
;; Bookmarks:1 ends here

;; [[file:config.org::*Keybindings within ibuffer mode][Keybindings within ibuffer mode:1]]
(map! :map ibuffer-mode-map
      (:prefix "f"
      :n "c" 'ibuffer-filter-by-content
      :n "d" 'ibuffer-filter-by-directory
      :n "f" 'ibuffer-filter-by-filename
      :n "m" 'ibuffer-filter-by-mode
      :n "n" 'ibuffer-filter-by-name
      :n "x" 'ibuffer-filter-disable)

      (:prefix "g"
      :n "h" 'ibuffer-do-kill-lines
      :n "H" 'ibuffer-update))
;; Keybindings within ibuffer mode:1 ends here

;; [[file:config.org::*Completion][Completion:1]]
(after! company
  (setq company-idle-delay nil))
;; Completion:1 ends here

;; [[file:config.org::*Completion][Completion:2]]
(advice-add #'doom-modeline-segment--modals :override #'ignore)
(setq doom-modeline-buffer-file-name-style 'file-name)
;; Completion:2 ends here

;; [[file:config.org::*Configuring Dashboard][Configuring Dashboard:1]]
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(map! :mode +doom-dashboard-mode
      :map +doom-dashboard-mode-map
      :desc "Find file" :ne "f" #'find-file
      :desc "Recent files" :ne "r" #'consult-recent-file
      :desc "Config dir" :ne "C" #'doom/open-private-config
      :desc "Open config.org" :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :desc "Open dotfile" :ne "." #'find-in-dotfiles
      :desc "Open suckless stuff" :ne "x" #'find-in-suckless
      :desc "Open scripts" :ne "e" #'find-in-scripts
      :desc "Notes (roam)" :ne "n" #'org-roam-node-find
      :desc "Dired" :ne "d" #'dired
      :desc "Switch buffer" :ne "b" #'+vertico/switch-workspace-buffer
      :desc "Switch buffers (all)" :ne "B" #'consult-buffer
      :desc "IBuffer" :ne "i" #'ibuffer
      :desc "Browse in project" :ne "p" #'doom/browse-in-other-project
      :desc "Set theme" :ne "t" #'consult-theme
      :desc "Mu4e" :ne "m" #'=mu4e
      :desc "Quit" :ne "Q" #'save-buffers-kill-terminal)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(map! :leader :desc "Dashboard" "e" #'+doom-dashboard/open)
;; (add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
;; (add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
;; (add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))
;; Configuring Dashboard:1 ends here

;; [[file:config.org::*File permissions and ownership][File permissions and ownership:1]]
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
(setq dired-open-extensions '(("gif" . "open")
                              ("jpg" . "open")
                              ("png" . "open")
                              ("mkv" . "open")
                              ("mp4" . "open")))
(setq find-file-visit-truename nil ;; Don't expand symlinks if you don't want to go insane.
      dired-kill-when-opening-new-dired-buffer t) ;; Kill the current buffer when selecting a new directory.
;; File permissions and ownership:1 ends here

;; [[file:config.org::*Keybindings Within Dired With Peep-Dired-Mode Enabled][Keybindings Within Dired With Peep-Dired-Mode Enabled:1]]
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Keybindings Within Dired With Peep-Dired-Mode Enabled:1 ends here

;; [[file:config.org::*Fonts and Appearance][Fonts and Appearance:1]]
(setq doom-font (font-spec :family "monospace" :size 20)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20)
      doom-big-font (font-spec :family "monospace" :size 34))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
;; Fonts and Appearance:1 ends here

;; [[file:config.org::*Fonts and Appearance][Fonts and Appearance:2]]
(setq doom-theme 'doom-dark+)
 ;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha 100)
(add-to-list 'default-frame-alist '(alpha 100))
;; Fonts and Appearance:2 ends here

;; [[file:config.org::*Fonts and Appearance][Fonts and Appearance:3]]
(custom-set-faces!
  '(mode-line :height 1.0)
  '(mode-line-inactive :height 0.9))
;; Fonts and Appearance:3 ends here

;; [[file:config.org::*Hooks][Hooks:1]]
(remove-hook 'text-mode-hook #'auto-fill-mode) ;; Prevent lines from auto breaking
;; Hooks:1 ends here

;; [[file:config.org::*Hooks][Hooks:2]]
(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
      (mu4e~start))
  (when (require 'elfeed nil t)
    (run-at-time nil (* 8 60 60) #'elfeed-update)))
(when (daemonp) ;; When the daemon is running
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup))
;; Hooks:2 ends here

;; [[file:config.org::*Key Mappings And Evil][Key Mappings And Evil:1]]
(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line))))
      :o "o" #'evil-inner-symbol
      :leader
      "h L" #'global-keycast-mode
      (:prefix "f"
       "t" #'find-in-dotfiles
       "T" #'browse-dotfiles)
      (:prefix "n"
       "L" #'org-latex-preview))

;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)
;; Implicit /g flag on evil ex substitution, because I use the default behavior less often.
(setq evil-ex-substitute-global t)
;; Key Mappings And Evil:1 ends here

;; [[file:config.org::*Line Settings][Line Settings:1]]
(setq display-line-numbers-type nil) ;; By disabling line number, we improve performance significantly
(map! :leader
     :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
    (:prefix ("t" . "toggle")
     :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
     :desc "Toggle line highlight in frame" "h" #'hl-line-mode
     :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
     :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))
;; Line Settings:1 ends here

;; [[file:config.org::*Latex][Latex:1]]
(after! latex-mode
  (TeX-engine-set 'luatex))
;; Latex:1 ends here

;; [[file:config.org::*Haskell][Haskell:1]]
(after! haskell-mode
  (set-formatter! 'brittany "brittany" :modes '(haskell-mode))
  (setq-hook! 'haskell-mode-hook +format-with-lsp nil))
;; Haskell:1 ends here

;; [[file:config.org::*Mouse Settings][Mouse Settings:1]]
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; Mouse Settings:1 ends here

;; [[file:config.org::*G-Mails][G-Mails:1]]
(set-email-account!
 "kylese58"
 '((user-full-name . "Kyle")
   (user-mail-address . "kylese58@gmail.com")
   (mu4e-sent-folder       . "/kylese58/[Gmail]/Sent Mail")
   (mu4e-trash-folder      . "/kylese58/[Gmail]/Bin")
   (mu4e-drafts-folder      . "/kylese58/[Gmail]/Drafts")
   (mu4e-refile-folder      . "/kylese58/[Gmail]/All Mail")
   (smtpmail-smtp-user     . "kylese58@gmail.com"))
 t)
(set-email-account!
 "entaroaldaris666"
 '((user-full-name . "Kyle Se")
   (user-mail-address . "entaroaldaris666@gmail.com")
   (mu4e-sent-folder       . "/entaroaldaris666/[Gmail]/Sent Mail")
   (mu4e-trash-folder      . "/entaroaldaris666/[Gmail]/Bin")
   (mu4e-drafts-folder      . "/entaroaldaris666/[Gmail]/Drafts")
   (mu4e-refile-folder      . "/entaroaldaris666/[Gmail]/All Mail")
   (smtpmail-smtp-user     . "entaroaldaris666@gmail.com"))
 nil)
(set-email-account!
 "vu"
 '((user-full-name . "Kyle Se Hin Son")
   (user-mail-address . "kyle.sehinson@live.vu.edu.au")
   (mu4e-sent-folder       . "/vu/Sent Items")
   (mu4e-trash-folder      . "/vu/Deleted Items")
   (mu4e-drafts-folder      . "/vu/Drafts")
   (mu4e-refile-folder      . "/vu/Archive")
   (smtpmail-smtp-user     . "kyle.sehinson@live.vu.edu.au"))
 nil)
(after! mu4e
  (setq mu4e-get-mail-command "mbsync -ac ~/.config/mbsync/config"
        mu4e-index-update-in-background t
        mu4e-update-interval 300 ;; get emails and index every 5 minutes
        mu4e-compose-format-flowed t ;; send emails with format=flowed
        ;; mu4e-index-cleanup nil ;; no need to run cleanup after indexing for gmail
        ;; mu4e-index-lazy-check t
        mu4e-use-fancy-chars t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-change-filenames-when-moving t
        mu4e-headers-date-format "%d.%m.%y" ;; more sensible date format
        ;; Org mu4e
        org-mu4e-convert-to-html t
        ))
;; G-Mails:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
;; Treemacs:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:2]]
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))
;; Treemacs:2 ends here

;; [[file:config.org::*Open Specific Files][Open Specific Files:1]]
(map! :leader
      (:prefix ("=" . "Open File")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/org/agenda.org"))
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))
       :desc "Edit xmonad xmonad.hs" "x" #'(lambda () (interactive) (find-file "~/.config/xmonad/xmonad.hs"))))
;; Open Specific Files:1 ends here

;; [[file:config.org::*Org][Org:1]]
(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (plist-put org-format-latex-options :scale 4) ;; Make latex equations preview larger
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "BLOG(b)"           ; Blog writing assignments
           "GYM(g)"            ; Things to accomplish at the gym
           "PROJ(p)"           ; A project that contains other tasks
           "VIDEO(v)"          ; Video assignments
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))) ; Task has been cancelled
;; Org:1 ends here

;; [[file:config.org::*Set font sizes for each header level in Org][Set font sizes for each header level in Org:1]]
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)
;; Set font sizes for each header level in Org:1 ends here

;; [[file:config.org::*Set font sizes for each header level in Org][Set font sizes for each header level in Org:2]]
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))
(add-hook 'org-mode-hook #'locally-defer-font-lock)
;; Set font sizes for each header level in Org:2 ends here

;; [[file:config.org::*Org-journal][Org-journal:1]]
(setq org-journal-dir (concat org-directory "/journal")
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")
;; Org-journal:1 ends here

;; [[file:config.org::*Org-roam][Org-roam:1]]
(after! org-roam
  (setq org-roam-directory (concat org-directory "/roam")
        org-roam-completion-everywhere t
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: < Inbox\n\n")
           :unnarrowed t)
          ("a" "articles" plain (file "~/org/templates/articles.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: + %^{Tag}\n\n")
           :unnarrowed t)
          ("b" "book notes" plain (file "~/org/templates/book.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: { %^{Tag}\n\n")
           :unnarrowed t)
          ("c" "podcasts" plain (file "~/org/templates/podcasts.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: @ %^{Tag}\n\n")
           :unnarrowed t)
          ("i" "ideas" plain (file "~/org/templates/ideas.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: > %^{Tag}\n\n")
           :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: - %^{Tag}\n\n")
           :unnarrowed t)
          ("p" "project" plain (file "~/org/templates/project.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: = %^{Tag}\n\n")
           :unnarrowed t)
          ("r" "research paper" plain (file "~/org/templates/research.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: ; %^{Tag}\n\n")
           :unnarrowed t)
          ("t" "tag" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Tag\n\n")
           :unnarrowed t)
          )))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t)
  ;; Add a new keybinding to open webview
  (map! :leader (:prefix ("n" . notes)
                 (:prefix ("r" . roam)
                  :desc "Open Web Graph" "w" #'org-roam-ui-mode))))
;; Org-roam:1 ends here

;; [[file:config.org::*Org-roam][Org-roam:2]]
(after! org-roam
  (add-hook! 'after-save-hook #'org-rename-to-new-title))
;; Org-roam:2 ends here

;; [[file:config.org::*Org Appear][Org Appear:1]]
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil
        org-appear-trigger 'manual)
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))
;; Org Appear:1 ends here

;; [[file:config.org::*Inkscape][Inkscape:1]]
(defvar ink-flags-png (list "--export-area-drawing"
                            "--export-dpi 100"
                            "--export-type=png"
                            "--export-background-opacity 1.0"
                            "--export-overwrite")
  "List of flags to produce a png file with inkspace.")

(defvar ink-default-file
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg
   width=\"297mm\"
   height=\"210mm\"
   viewBox=\"0 0 297 210\"
   version=\"1.1\"
   id=\"svg8\"
   inkscape:version=\"1.1.2 (0a00cf5339, 2022-02-04, custom)\"
   sodipodi:docname=\"default.svg\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
  <defs
     id=\"defs2\">
    <rect
       x=\"160\"
       y=\"60\"
       width=\"40\"
       height=\"10\"
       id=\"rect121\" />
    <rect
       x=\"150\"
       y=\"70\"
       width=\"50\"
       height=\"10\"
       id=\"rect115\" />
    <rect
       x=\"140\"
       y=\"50\"
       width=\"90\"
       height=\"30\"
       id=\"rect109\" />
    <rect
       x=\"170\"
       y=\"70\"
       width=\"70\"
       height=\"50\"
       id=\"rect97\" />
    <rect
       x=\"129.26784\"
       y=\"79.883835\"
       width=\"85.494354\"
       height=\"60.623272\"
       id=\"rect47\" />
  </defs>
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"1\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"0.93616069\"
     inkscape:cx=\"515.93707\"
     inkscape:cy=\"205.093\"
     inkscape:document-units=\"mm\"
     inkscape:current-layer=\"g75\"
     showgrid=\"true\"
     showborder=\"true\"
     width=\"1e-05mm\"
     showguides=\"true\"
     inkscape:guide-bbox=\"true\"
     inkscape:window-width=\"1882\"
     inkscape:window-height=\"1012\"
     inkscape:window-x=\"1382\"
     inkscape:window-y=\"46\"
     inkscape:window-maximized=\"0\"
     inkscape:document-rotation=\"0\"
     inkscape:pagecheckerboard=\"0\"
     units=\"mm\">
    <inkscape:grid
       type=\"xygrid\"
       id=\"grid815\"
       units=\"mm\"
       spacingx=\"10\"
       spacingy=\"10\"
       empspacing=\"4\"
       dotted=\"false\" />
  </sodipodi:namedview>
  <metadata
     id=\"metadata5\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\"
     transform=\"translate(0,-177)\" />
  <g
     inkscape:label=\"Capacitor\"
     transform=\"rotate(-90,90,60)\"
     id=\"g27\">
    <text
       xml:space=\"preserve\"
       id=\"text45\"
       style=\"font-size:20;line-height:1.25;font-family:Sans;-inkscape-font-specification:'Sans, Normal';letter-spacing:0px;white-space:pre;shape-inside:url(#rect47)\" />
  </g>
  <g
     inkscape:label=\"Capacitor\"
     id=\"g75\">
    <text
       xml:space=\"preserve\"
       id=\"text95\"
       style=\"font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:20px;font-family:Sans;-inkscape-font-specification:'Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal;text-align:center;white-space:pre;shape-inside:url(#rect97);fill:none;stroke:#000000;stroke-width:1;stroke-linecap:round;stroke-linejoin:round;stroke-dasharray:4, 8;paint-order:fill markers stroke\" />
  </g>
</svg>"
  "Default file template.")
;; Inkscape:1 ends here

;; [[file:config.org::*Which-key][Which-key:1]]
(after! which-key
  (setq which-key-idle-delay 0.2))
;; Which-key:1 ends here

;; [[file:config.org::*Which-key][Which-key:2]]
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;; Which-key:2 ends here
