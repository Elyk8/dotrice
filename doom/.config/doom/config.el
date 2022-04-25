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
      doom-unicode-font (font-spec :family "JoyPixels" :size 20)
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

(setenv "SHELL" "/bin/zsh")
(after! tramp
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

(use-package! i3wm-config-mode
  :defer t)

(use-package! transpose-frame
  :commands (transpose-frame))

(defun elk/emacs-i3-windmove (dir)
  (let ((other-window (windmove-find-other-window dir)))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (- (error dir))
      (windmove-do-window-select dir))))

(defun elk/emacs-i3-direction-exists-p (dir)
  (some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun elk/emacs-i3-move-window (dir)
  (let ((other-window (windmove-find-other-window dir))
        (other-direction (elk/emacs-i3-direction-exists-p
                          (pcase dir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window dir))
     (t (error dir)))))

(defun elk/emacs-i3-resize-window (dir kind value)
  (if (or (one-window-p)
          (not (elk/emacs-i3-direction-exists-p dir)))
      (- (error (concat (symbol-name kind) (symbol-name dir))))
    (setq value (/ value 2))
    (pcase kind
      ('shrink
       (pcase dir
         ('width
          (evil-window-decrease-width value))
         ('height
          (evil-window-decrease-height value))))
      ('grow
       (pcase dir
         ('width
          (evil-window-increase-width value))
         ('height
          (evil-window-increase-height value)))))))

(defun elk/emacs-i3-integration (command)
  (pcase command
    ((rx bos "focus")
     (elk/emacs-i3-windmove
      (intern (elt (split-string command) 1))))
    ((rx bos "move")
     (elk/emacs-i3-move-window
      (intern (elt (split-string command) 1))))
    ((rx bos "resize")
     (elk/emacs-i3-resize-window
       (intern (elt (split-string command) 2))
       (intern (elt (split-string command) 1))
       (string-to-number (elt (split-string command) 3))))
    ("layout toggle split" (transpose-frame))
    ("split v" (evil-window-split))
    ("split h" (evil-window-vsplit))
    ("kill" (evil-quit))
    (- (error command))))

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

(use-package! pomm
  :commands (pomm)
  :config
  (setq alert-default-style 'libnotify
        pomm-audio-enabled t
        pomm-audio-player-executable "pomodoro-play"
        pomm-audio-files '((work . "/home/elyk/.dotrice/applications/.local/share/sounds/work.wav" )
                           (tick . "/home/elyk/.emacs.d/.local/straight/build-28.1/pomm/resources/tick.wav")
                           (short-break . "/home/elyk/.dotrice/applications/.local/share/sounds/break.wav")
                           (long-break . "home/elyk/.dotrice/applications/.local/share/sounds/break.wav")
                           (stop . "/home/elyk/.emacs.d/.local/straight/build-28.1/pomm/resources/tick.wav")))
  (add-hook 'pomm-on-tick-hook 'pomm-update-mode-line-string)
  (add-hook 'pomm-on-status-changed-hook 'pomm-update-mode-line-string))
(map! (:leader
       :prefix ("t")
       :desc "Pomodoro" :n "t" #'pomm))

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
(elk/add-file-keybinding "i" "~/.config/i3/i3.org" "i3 i3.org")
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
