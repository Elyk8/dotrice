;;; config.el -*- lexical-binding: t; -*-

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

(setq-default
 delete-by-moving-to-trash t                    ; Delete files to trash
 window-combination-resize t                    ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                            ; Stretch cursor to the glyph width

(setq undo-limit 80000000                       ; Raise undo-limit to 80Mb
      display-line-numbers-type nil             ; By disabling line number, we improve performance significantly
      evil-want-fine-undo t                     ; By default while in insert all changes are one big blob. Be more granular
      truncate-string-ellipsis "…"              ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                 ; I can trust my computers ... can't I?
      scroll-margin 2                           ; It's nice to maintain a little margin
      confirm-kill-emacs nil                    ; Disable exit confirmation
      )

;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; Prevents some cases of Emacs flickering.

;; Improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))      ; one line at a time
      mouse-wheel-progressive-speed nil                 ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                       ; scroll window under mouse
      scroll-preserve-screen-position 'always           ; Don't have `point' jump around
      scroll-step 1)                                    ; keyboard scroll one line at a time

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

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

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(setq doom-font (font-spec :family "monospace" :size 20)
      doom-variable-pitch-font (font-spec :family "sans" :size 20)
      doom-big-font (font-spec :family "monospace" :size 34))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-vibrant)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(remove-hook 'text-mode-hook #'auto-fill-mode) ;; Prevent lines from auto breaking

(setq! citar-library-paths '("~/dox/bibliography/")
       citar-notes-paths "~/dox/notes/")

(use-package! company
  :after-call (company-mode global-company-mode company-complete
                            company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-idle-delay nil
        company-tooltip-limit 10))

(advice-add #'doom-modeline-segment--modals :override #'ignore)

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2))
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;; (add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
;; (setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(map! :leader :desc "Dashboard" "e" #'+doom-dashboard/open)
;; (add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
;; (add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
;; (add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))

(add-hook! 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook! 'dired-mode 'dired-async-mode)

(setq dired-open-extensions '(("gif" . "open")
                              ("jpg" . "open")
                              ("png" . "open")
                              ("mkv" . "open")
                              ("mp4" . "open")))
(setq find-file-visit-truename nil ;; Don't expand symlinks if you don't want to go insane.
      dired-kill-when-opening-new-dired-buffer t) ;; Kill the current buffer when selecting a new directory.

(setq ispell-dictionary "en-custom"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        ;;evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil) ; Don't put overwritten text in the kill ring
  ;; Focus new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(defun calendar-helper () ;; doesn't have to be interactive
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Purple")
    (cfw:ical-create-source "Victoria University" "https://outlook.office365.com/owa/calendar/14853855dd6541eebbce1f2d68f50dcf@live.vu.edu.au/f754347027b54d97a148bdb20e6a947814803601956198516593/calendar.ics" "Green"))))
(defun calendar-init ()
  ;; switch to existing calendar buffer if applicable
  (if-let (win (cl-find-if (lambda (b) (string-match-p "^\\*cfw:" (buffer-name b)))
                           (doom-visible-windows)
                           :key #'window-buffer))
      (select-window win)
    (calendar-helper)))

(defun =my-calendar ()
  "Activate (or switch to) *my* `calendar' in its workspace."
  (interactive)
  (if (featurep! :ui workspaces) ;; create workspace (if enabled)
      (progn
        (+workspace-switch "Calendar" t)
        (doom/switch-to-scratch-buffer)
        (calendar-init)
        (+workspace/display))
    (setq +calendar--wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))
    (calendar-init)))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'auto
        all-the-icons-scale-factor 1.1
        ;;doom-modeline-enable-word-count t         ; Show word count in modeline
        inhibit-compacting-font-caches t          ; Don’t compact font caches during GC.
        find-file-visit-truename t))              ; Display true name instead of relative name

(custom-set-faces!
  '(mode-line :height 1.0)
  '(mode-line-inactive :height 1.0))

(after! org
  (plist-put org-format-latex-options :scale 4) ;; Make latex equations preview larger
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t
        org-insert-heading-respect-content nil ;; Insert org headings at point
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "INPROCESS(s)"  ; A task that is in progress
           "⚑ WAITING(w)"  ; Something is holding up this task; or it is paused
           "|"
           "☟ NEXT(n)"
           "✰ IMPORTANT(i)"
           "DONE(d)"  ; Task successfully completed
           "✘ CANCELED(c@)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "✍ NOTE(N)"
           "FIXME(f)"
           "☕ BREAK(b)"
           "❤ LOVE(l)"
           "REVIEW(r)"
           )) ; Task was completed
        org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "#ff39a3" :weight bold))
          ("INPROCESS"  . "orangered")
          ("✘ CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("⚑ WAITING" . "pink")
          ("☕ BREAK" . "gray")
          ("❤ LOVE" . (:foreground "VioletRed4"
                       ;; :background "#7A586A"
                       :weight bold))
          ("☟ NEXT" . (:foreground "DeepSkyBlue"
                       ;; :background "#7A586A"
                       :weight bold))
          ("✰ IMPORTANT" . (:foreground "greenyellow"
                            ;; :background "#7A586A"
                            :weight bold))
          ("DONE" . "#008080")
          ("FIXME" . "IndianRed")))) ; Task has been cancelled

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
  ;; (setq org-superstar-headline-bullets-list '("一" "二" "三" "四" "五" "六" "七" "八")
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-superstar-prettify-item-bullets t ))

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(after! org
  (add-hook 'org-mode-hook #'locally-defer-font-lock))

(setq org-odt-preferred-output-format "docx")

(after! org-journal
  (setq org-journal-dir (concat org-directory "journal")
        org-journal-date-prefix "* "
        org-journal-time-prefix "** "
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"))

(after! org-roam
  (setq org-roam-directory "~/org/roam"
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
          ("P" "presentation" plain (file "~/org/templates/presentation.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "\n:reveal_properties:\n#+reveal_root: https://cdn.jsdelivr.net/npm/reveal.js\n:end:\n\n#+title: ${title}\n#+date: %U\n#+author: %^{Author}\n#+filetags: < Presentation\n\n")
           :unnarrowed t)
          ("r" "research paper" plain (file "~/org/templates/research.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: ; %^{Tag}\n\n")
           :unnarrowed t)
          ("t" "tag" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Tag\n\n")
           :unnarrowed t)
          )))

(after! org-roam
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

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

(after! org-roam
  (add-hook! 'after-save-hook #'elk/org-roam-rename-to-new-title))

(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)

(after! eshell
  (setq eshell-rc-script "~/.config/doom/eshell/profile"
        eshell-aliases-file "~/.config/doom/eshell/aliasrc"
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "xsh" "htop" "ssh" "top" "fish")))

(after! org
  (require 'ox-taskjuggler))

(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>\n]*#?[]#$%>] *\\(�\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 

(after! vterm
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off"))

(after! which-key
  (setq which-key-allow-imprecise-window-fit t) ; Comment this if experiencing crashes
  ;; Add an extra line to work around bug in which-key imprecise
  (defun add-which-key-line (f &rest r) (progn (apply f (list (cons (+ 1 (car (car r))) (cdr (car r)))))))
  (advice-add 'which-key--show-popup :around #'add-which-key-line)
  (setq which-key-idle-delay 0.2))

(defun elk/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun elk/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "$HOME/.fehbg"))

(defun elk/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Start polybar
  (elk/start-panel))

  ;; Open eshell by default
  ;; (+eshell/here))

(defun elk/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun elk/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun elk/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-move-window 2))
    ("Chromium" (exwm-workspace-move-window 2))
    ("discord" (exwm-workspace-move-window 3))
    ("Virt-manager" (exwm-workspace-move-window 5))
    ("mpv" (exwm-floating-toggle-floating)
     (elk/exwm-floating-toggle-pinned))
    ))

;; This function should be used only after configuring autorandr!
(defun elk/update-displays ()
  (elk/run-in-background "autorandr --change --force")
  (elk/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(defun elk/fix-exwm-floating-windows ()
  (setq-local exwm-workspace-warp-cursor nil
              mouse-autoselect-window nil
              focus-follows-mouse nil))

(setq elk/exwm-last-workspaces '(1))

(defun elk/exwm-store-last-workspace ()
  "Save the last workspace to `elk/exwm-last-workspaces'."
  (setq elk/exwm-last-workspaces
        (seq-uniq (cons exwm-workspace-current-index
                        elk/exwm-last-workspaces))))

(defun elk/exwm-last-workspaces-clear ()
  "Clean `elk/exwm-last-workspaces' from deleted workspaces."
  (setq elk/exwm-last-workspaces
        (seq-filter
         (lambda (i) (nth i exwm-workspace--list))
         elk/exwm-last-workspaces)))

(setq elk/exwm-monitor-list '(nil "HDMI-1-0"))

(defun elk/exwm-get-current-monitor ()
  "Return the current monitor name or nil."
  (plist-get exwm-randr-workspace-output-plist
             (cl-position (selected-frame)
                          exwm-workspace--list)))

(defun elk/exwm-get-other-monitor (dir)
  "Cycle the monitor list in the direction DIR.

DIR is either 'left or 'right."
  (nth
   (%    (+ (cl-position
             (elk/exwm-get-current-monitor)
             elk/exwm-monitor-list
             :test #'string-equal)
            (length elk/exwm-monitor-list)
            (pcase dir
              ('right 1)
              ('left -1)))
         (length elk/exwm-monitor-list))
   elk/exwm-monitor-list))

(defun elk/exwm-switch-to-other-monitor (&optional dir)
  "Switch to another monitor."
  (interactive)
  (elk/exwm-last-workspaces-clear)
  (exwm-workspace-switch
   (cl-loop with other-monitor = (elk/exwm-get-other-monitor (or dir 'right))
            for i in (append elk/exwm-last-workspaces
                             (cl-loop for i from 0
                                      for _ in exwm-workspace--list
                                      collect i))
            if (if other-monitor
                   (string-equal (plist-get exwm-randr-workspace-output-plist i)
                                 other-monitor)
                 (not (plist-get exwm-randr-workspace-output-plist i)))
            return i)))

(defun elk/exwm-workspace-switch-monitor ()
  "Move the current workspace to another monitor."
  (interactive)
  (let ((new-monitor (elk/exwm-get-other-monitor 'right))
        (current-monitor (elk/exwm-get-current-monitor)))
    (when (and current-monitor
               (>= 1
                   (cl-loop for (key value) on exwm-randr-workspace-monitor-plist
                            by 'cddr
                            if (string-equal value current-monitor) sum 1)))
      (error "Can't remove the last workspace on the monitor!"))
    (setq exwm-randr-workspace-monitor-plist
          (map-delete exwm-randr-workspace-monitor-plist exwm-workspace-current-index))
    (when new-monitor
      (setq exwm-randr-workspace-monitor-plist
            (plist-put exwm-randr-workspace-monitor-plist
                       exwm-workspace-current-index
                       new-monitor))))
  (exwm-randr-refresh))

(defun elk/exwm-windmove (dir)
  "Move to window or monitor in the direction DIR."
  (if (or (eq dir 'down) (eq dir 'up))
      (windmove-do-window-select dir)
    (let ((other-window (windmove-find-other-window dir))
          (other-monitor (elk/exwm-get-other-monitor dir))
          (opposite-dir (pcase dir
                          ('left 'right)
                          ('right 'left))))
      (if other-window
          (windmove-do-window-select dir)
        (elk/exwm-switch-to-other-monitor dir)
        (cl-loop while (windmove-find-other-window opposite-dir)
                 do (windmove-do-window-select opposite-dir))))))

(defun elk/exwm-fill-other-window (&rest _)
  "Open the most recently used buffer in the next window."
  (interactive)
  (when (and (eq major-mode 'exwm-mode) (not (eq (next-window) (get-buffer-window))))
    (let ((other-exwm-buffer
           (cl-loop with other-buffer = (persp-other-buffer)
                    for buf in (sort (persp-current-buffers) (lambda (a _) (eq a other-buffer)))
                    with current-buffer = (current-buffer)
                    when (and (not (eq current-buffer buf))
                              (buffer-live-p buf)
                              (not (string-match-p (persp--make-ignore-buffer-rx) (buffer-name buf)))
                              (not (get-buffer-window buf)))
                    return buf)))
      (when other-exwm-buffer
        (with-selected-window (next-window)
          (switch-to-buffer other-exwm-buffer))))))

(defvar elk/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun elk/kill-panel ()
  (interactive)
  (when elk/polybar-process
    (ignore-errors
      (kill-process elk/polybar-process)))
  (setq elk/polybar-process nil))

(defun elk/start-panel ()
  (interactive)
  (elk/kill-panel)
  (setq elk/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun elk/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun elk/send-polybar-exwm-workspace ()
  (elk/send-polybar-hook "exwm-workspace" 1))

(defun elk/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "0")
    (1 "1")
    (2 "2")
    (3 "3")
    (4 "4")
    (5 "5")))

(use-package! exwm-randr
  :after exwm
  :config
  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "multi-hybrid-graphics")

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  (setq exwm-randr-workspace-monitor-plist '(2 "HDMI-1-0" 3 "HDMI-1-0"))

  ;; NOTE: Uncomment these lines after setting up autorandr!
  ;; React to display connectivity changes, do initial display update
  (add-hook 'exwm-randr-screen-change-hook #'elk/update-displays)
  (elk/update-displays)

  ;; Set the wallpaper after changing the resolution
  (elk/set-wallpaper))

(use-package! app-launcher
  :commands (app-launcher-run-app))

(use-package! desktop-environment
  :after exwm
  :diminish desktop-environment-mode
  :config
  (progn
    (unbind-key "s-l" desktop-environment-mode-map)
    (desktop-environment-mode))
  :custom
  (desktop-environment-volume-get-command "volume")
  (desktop-environment-volume-get-regexp "^\\([0-9]+\\)")
  (desktop-environment-volume-set-command "volume %s")
  (desktop-environment-volume-normal-increment "up")
  (desktop-environment-volume-normal-decrement "down")
  (desktop-environment-volume-small-increment "sup")
  (desktop-environment-volume-small-decrement "sdown")
  (desktop-environment-volume-toggle-command "volume mute")
  (desktop-environment-volume-toggle-microphone-command "mic-toggle")

  (desktop-environment-brightness-get-command "brightness")
  (desktop-environment-brightness-set-command "brightness %s")
  (desktop-environment-brightness-get-regexp "^\\([0-9]+\\)")
  (desktop-environment-brightness-normal-increment "up")
  (desktop-environment-brightness-normal-decrement "down")
  (desktop-environment-brightness-small-increment "sup")
  (desktop-environment-brightness-small-decrement "sdown")

  (desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-screenshot-directory (concat (getenv "HOME") "/pix/screenshots")))

(use-package! exwm
  :init
  (setq exwm-workspace-warp-cursor t
        mouse-autoselect-window t
        focus-follows-mouse t)          ; Window focus should follow the mouse pointer
  (server-start)                        ; Start the emacs server
  (setq exwm-workspace-number 6)        ; Set the default number of workspaces

  :config
  (add-hook 'exwm-update-class-hook #'elk/exwm-update-class) ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'elk/exwm-update-title) ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-manage-finish-hook #'elk/configure-window-by-class) ;; Configure windows as they're created
  (add-hook 'exwm-init-hook #'elk/exwm-init-hook) ;; When EXWM starts up, do some extra confifuration

  ;; NOTE: Uncomment the following two options if you want window buffers
  ;;       to be available on all workspaces!

  ;;(setq exwm-layout-show-all-buffers t) ;; Automatically move EXWM buffer to current workspace when selected
  ;;(setq exwm-workspace-show-all-buffers t) ;; Display all EXWM buffers in every workspace buffer list

  ;; NOTE: Uncomment this option if you want to detach the minibuffer!
  ;;(setq exwm-workspace-minibuffer-position 'top) ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)

  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h) ;; Show `exwm' buffers in buffer switching prompts.
  (add-hook 'exwm-workspace-switch-hook #'elk/exwm-store-last-workspace) ;; Swapping workspaces between monitors
  (add-hook 'exwm-floating-setup-hook #'elk/fix-exwm-floating-windows) ;; For floating windows, this will break EXWM. So we disable the above for floating mode.
  (add-hook 'exwm-workspace-switch-hook #'elk/send-polybar-exwm-workspace) ;; Update panel indicator when workspace changes

  (add-hook 'exwm-manage-finish-hook #'(lambda () (interactive) (evil-insert-state))) ;; This allows the use of doom alternate leader key in x windows

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\M-\     ;; Alt+Space
          ?\s-\ ))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Find a better window for the split
  (advice-add 'evil-window-split :after #'elk/exwm-fill-other-window)
  (advice-add 'evil-window-vsplit :after #'elk/exwm-fill-other-window)

  ;; Super + space prefix key for general keybindings
  (map! (:prefix "s-SPC"
         :desc "System activity" "q" #'(lambda() (interactive) (elk/run-in-background "sysact"))
         "b" #'switch-to-buffer
         "SPC" #'app-launcher-run-app
         :desc "Launch Firefox" "w" #'(lambda() (interactive) (elk/run-in-background "prime-run firefox"))
         :desc "Launch Chromium" "c" #'(lambda() (interactive) (elk/run-in-background "prime-run chromium"))
         "d" #'dmenu
         "e" #'+eshell/here
         :desc "Launch alacritty terminal" "t" #'(lambda() (interactive) (elk/run-in-background (getenv "TERMINAL") ))
         :desc "Terminal Launch lf" "r" #'(lambda() (interactive) (elk/run-in-background (concat (getenv "TERMINAL") " -e lf") ))
         :desc "Terminal Launch ncmpcpp" "n" #'(lambda() (interactive) (elk/run-in-background (concat (getenv "TERMINAL") " -e ncmpcpp") ))
         :desc "Mute/Unmute microphone" "m" #'(lambda() (interactive) (elk/run-in-background "mic-toggle"))
         (:prefix ("o" . "Other Applications")
          :desc "Launch discord" "d" #'(lambda() (interactive) (elk/run-in-background "discord"))
          :desc "Launch FTB" "f" #'(lambda() (interactive) (elk/run-in-background "/media/FTBA/FTBApp"))
          :desc "Launch Zotero" "t" #'(lambda() (interactive) (elk/run-in-background "zotero"))
          :desc "Launch Zoom" "z" #'(lambda() (interactive) (elk/run-in-background "zoom")))
         (:prefix ("p" . "Dmenu Scripts")
          :desc "Select man pages" "a" #'(lambda() (interactive) (elk/run-in-background "dm-man"))
          :desc "Clipmenu" "c" #'(lambda() (interactive) (elk/run-in-background "clipmenu"))
          :desc "Change colorscheme" "C" #'(lambda() (interactive) (elk/run-in-background "dm-colorscheme"))
          :desc "Kill selected application" "k" #'(lambda() (interactive) (elk/run-in-background "dm-kill"))
          :desc "Mount drives" "o" #'(lambda() (interactive) (elk/run-in-background "dm-mount"))
          :desc "Unmount drives" "u" #'(lambda() (interactive) (elk/run-in-background "dm-umount"))
          :desc "Passmenu" "p" #'(lambda() (interactive) (elk/run-in-background "dm-passmenu"))
          :desc "FM Radio" "b" #'(lambda() (interactive) (elk/run-in-background "dm-beats"))
          :desc "Weather forecast" "w" #'(lambda() (interactive) (elk/run-in-background "weatherforecast")))
         (:prefix (";" . "System settings")
          :desc "Set wallpaper from a2n gallery" "a" #'(lambda() (interactive) (elk/run-in-background "setwallpaper a2n"))
          :desc "Set wallpaper from dt gallery" "d" #'(lambda() (interactive) (elk/run-in-background "setwallpaper dt"))
          :desc "Set wallpaper from elyk gallery" "e" #'(lambda() (interactive) (elk/run-in-background "setwallpaper elyk"))
          :desc "Open pulsemixer" "v" #'(lambda() (interactive) (elk/run-in-background (concat (getenv "TERMINAL") " -e pulsemixer") )))))

  ;; Set global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Splits
          ([?\s-v] . evil-window-vsplit)
          ([?\s-z] . evil-window-split)

          ;; Switch workspace
          ([?\s-w] . (lambda () (interactive) (elk/exwm-switch-to-other-monitor)))
          ([?\s-W] . (lambda () (interactive) (elk/exwm-workspace-switch-monitor)))
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; Change layouts
          ([?\s-b] . (lambda () (interactive) (rotate-layout)))

          ;; Killing buffers and windows
          ([?\s-c] . kill-current-buffer)
          ([?\s-q] . +workspace/close-window-or-workspace)

          ;; Change focus between windows
          ([?\s-h] . evil-window-left)
          ([?\s-j] . evil-window-next)
          ([?\s-k] . evil-window-prev)
          ([?\s-l] . evil-window-right)

          ;; Move windows around
          ([?\s-H] . +evil/window-move-left)
          ([?\s-J] . +evil/window-move-down)
          ([?\s-K] . +evil/window-move-up)
          ([?\s-L] . +evil/window-move-right)

          ([?\s-g] . exwm-floating-toggle-floating)
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-m] . exwm-layout-toggle-mode-line)
          ([?\s-i] . exwm-input-toggle-keyboard) ;; Toggle between "line-mode" and "char-mode" in an EXWM window

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))

          ,@(cl-mapcar (lambda (c n)
                         `(,(kbd (format "s-%c" c)) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-move-window ,n)
                             (exwm-workspace-switch ,n))))
                       '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                       ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                       (number-sequence 0 9))))

  (add-hook 'exwm-input--input-mode-change-hook 'force-mode-line-update)

  (exwm-enable))

(use-package! edraw-org
  :after org
  :config
  (edraw-org-setup-default))

(use-package! kbd-mode
  :defer t)

(use-package! dmenu
  :commands (dmenu dmenu-save-to-file))

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

(use-package! ox-moderncv
  :after org)

(defun elk/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :map org-mode-map
        :leader
        (:prefix ("m a")
        "p" #'elk/org-download-paste-clipboard)))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))

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

(use-package! org-appear
  :after org
  :hook (org-mode . (lambda ()
                      (org-appear-mode t)
                      (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
                      (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil
        org-appear-trigger 'manual)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! org-archive
  :after org
  :config
  (setq org-archive-location "archive.org::datetree/"))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-dir-name t)))
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary nil
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t)
  (org-super-agenda-mode))

(use-package! stumpwm-mode
  :defer t
  :hook lisp-mode)

(use-package! sly
  :defer t)

(use-package! emacs-powerthesaurus
  :after-call (powerthesaurus-lookup-synonyms-dwim
               powerthesaurus-lookup-antonyms-dwim powerthesaurus-lookup-related-dwim
               powerthesaurus-lookup-definitions-dwim powerthesaurus-lookup-sentences-dwim ))

(use-package! company-english-helper
  :after company)

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

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

(map! :mode +doom-dashboard-mode
      :map +doom-dashboard-mode-map
      :desc "Find file" :ne "f" #'find-file
      :desc "Recent files" :ne "r" #'consult-recent-file
      :desc "Open config.org" :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :desc "Config dir" :ne "C" #'doom/open-private-config
      :desc "Open dotfile" :ne "." #'find-in-dotfiles
      :desc "Open configs" :ne ">" #'find-in-configs
      :desc "Open suckless stuff" :ne "x" #'find-in-suckless
      :desc "Open scripts" :ne "e" #'find-in-scripts
      :desc "Notes (roam)" :ne "n" #'org-roam-node-find
      :desc "Dired" :ne "d" #'dired
      :desc "Switch buffer" :ne "b" #'+vertico/switch-workspace-buffer
      :desc "Switch buffers (all)" :ne "B" #'consult-buffer
      :desc "IBuffer" :ne "i" #'ibuffer
      :desc "Browse in project" :ne "p" #'doom/browse-in-other-project
      :desc "Set theme" :ne "t" #'consult-theme
      :desc "Quit" :ne "Q" #'save-buffers-kill-terminal)

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
  (kbd "Y") 'dired-copy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-o-encrypt)

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(after! evil
  (map! :nv "Q" #'evil-fill-and-move))

(after! evil
  (defun elk/ex-kill-buffer-and-close ()
    (interactive)
    (unless (char-equal (elt (buffer-name) 0) ?*)
      (kill-this-buffer)))

  (defun elk/ex-save-kill-buffer-and-close ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))

  (evil-ex-define-cmd "q[uit]" 'elk/ex-kill-buffer-and-close )
  (evil-ex-define-cmd "wq" 'elk/ex-save-kill-buffer-and-close))

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(map! :leader
    (:prefix ("t" . "toggle")
     :desc "Comment or uncomment lines" "/" #'comment-line
     :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
     :desc "Toggle line highlight in frame" "h" #'hl-line-mode
     :desc "Toggle line highlight globally" "H" #'global-hl-line-mode))

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

(map! :map org-mode-map
      :leader
      :desc "Org babel tangle" "m TAB" #'org-babel-tangle)

(defun elk/add-file-keybinding (key file &optional desc)
  (let ((key key)
        (file file)
        (desc desc))
    (map! :leader
          (:prefix ("-" . "Open File")
           :desc (or desc file)
           key
           #'(lambda () (interactive) (find-file file))))))

(defun elk/add-project-keybinding (key file &optional desc)
  (let ((key key)
        (file file)
        (desc desc))
    (map! :leader
          (:prefix ("=" . "Open Project")
           :desc (or desc file)
           key
           #'(lambda () (interactive) (doom-project-find-file file))))))

(elk/add-file-keybinding "a" "~/org/agenda.org" "Agenda agenda.org")
(elk/add-file-keybinding "f" "~/.config/fontconfig/fonts.conf" "Fonts config fonts.conf")
(elk/add-file-keybinding "s" "~/.config/sxhkd/sxhkdrc.org" "Sxhkdrc sxhkdrc.org")
(elk/add-file-keybinding "k" "~/.config/kmonad/kmonad.kbd" "Kmonad kmonad.kbd")
(elk/add-file-keybinding "d" (expand-file-name "config.org" doom-private-dir) "Doom config.org")
(elk/add-file-keybinding "x" "~/.config/xmonad/xmonad.hs" "Xmonad xmonad.hs")

(elk/add-project-keybinding "d" "~/.config/doom/" "Doom")
(elk/add-project-keybinding "s" "~/.config/shell/" "Shell")
(elk/add-project-keybinding "x" "~/.config/xmonad/" "Xmonad")
(elk/add-project-keybinding "z" "~/.config/zsh/" "Zsh")

(setq +format-with-lsp nil)

(setq-default TeX-engine 'luatex)

(set-formatter! 'stylua "stylua -" :modes '(lua-mode))

(set-formatter! 'brittany "brittany" :modes '(haskell-mode))

(set-formatter! 'autopep8 "autopep8 -" :modes '(python-mode))
