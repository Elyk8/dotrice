;; -*- lexical-binding: t; -*-

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

(setq org-directory (file-truename "~/Documents/org"))
(setq org-roam-directory org-directory)
(setq doom-localleader-key ",")

;; Make writing and scrolling faster
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 30000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(setq-default
 ;; evil-cross-lines t                             ;; Make horizontal movement cross lines
 delete-by-moving-to-trash t                    ;; Delete files to trash
 window-combination-resize t                    ;; take new window space from all other windows (not just current)
 x-stretch-cursor t)                            ;; Stretch cursor to the glyph width

(setq undo-limit 80000000                       ;; Raise undo-limit to 80Mb
      display-line-numbers-type nil             ;; By disabling line number, we improve performance significantly
      ;; evil-want-fine-undo t                     ;; By default while in insert all changes are one big blob. Be more granular
      truncate-string-ellipsis "\u2026"         ;; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                 ;; I can trust my computers ... can't I?
      confirm-kill-emacs nil                    ;; Disable exit confirmation
      )

;; Make evil more like vim behaviour
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; Prevents some cases of Emacs flickering.

;; Improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))      ;; one line at a time
      mouse-wheel-progressive-speed nil                 ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                       ;; scroll window under mouse
      scroll-preserve-screen-position 'always           ;; Don't have `point' jump around
      scroll-step 1)                                    ;; keyboard scroll one line at a time

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with elisp or note something down (that isn't worth an entry in my notes). I can do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Prevent lines from auto breaking
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Automatically revert buffers for changed files
(setq global-auto-revert-non-file-buffers t)

(after! gcmh
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 255 1024 1024)))

(setq inhibit-compacting-font-caches nil)

;;; Fonts
(setq doom-font (font-spec :family "JetBrains Mono" :size 22)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 22)
      doom-unicode-font (font-spec :family "Noto Color Emoji" :size 22)
      doom-big-font (font-spec :family "JetBrains Mono" :size 34))

;; Themes
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange")
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-dark+)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Make fill-paragraph (M-q) 100 characters long
;;(setq-default fill-column 100)

(setq visual-fill-column-width 100
      visual-fill-column-center-text t)

(after! which-key
  (setq which-key-idle-delay 0.2
        ;; Comment this if experiencing crashes
        which-key-allow-imprecise-window-fit t))

(map! :i
      "C-SPC" #'completion-at-point)

(map! :map visual-line-mode-map
      :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)
(map! (:after evil-org
       :map evil-org-mode-map
       :nv "j" 'evil-next-visual-line
       :nv "k" 'evil-previous-visual-line
       :nv "gk" (cmd! (if (org-on-heading-p)
                          (org-backward-element)
                        (evil-previous-visual-line)))
       :nv "gj" (cmd! (if (org-on-heading-p)
                          (org-forward-element)
                        (evil-next-visual-line))))
      :o "o" #'evil-inner-symbol)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired"            "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        "d p" #'peep-dired :desc "Peep-dired image previews"
        "d v" #'dired-view-file :desc "Dired view file")))
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

(defun elk/open-file (file)
  (interactive)
  (find-file file))

(map! :leader
      :prefix-map ("e" . "Elyk")
      :desc "Open agenda.org"  "a" '(elk/open-file (concat org-directory "/agenda.org"))
      :desc "Open elfeed.org"  "e" '(elk/open-file (concat org-directory "/elfeed.org"))
      :desc "Open fonts.conf"  "f" '(elk/open-file "~/.config/fontconfig/fonts.conf")
      :desc "Open i3.org"      "i" '(elk/open-file "~/.config/i3/i3.org")
      :desc "Open polybar.org" "p" '(elk/open-file "~/.config/polybar/polybar.org")
      :desc "Open sxhkd.org"   "s" '(elk/open-file "~/.config/sxhkd/sxhkdrc.org")
      :desc "Open x.org"       "x" '(elk/open-file "~/.config/x11/x.org")
      )

(map! :leader
      (:prefix ("t" . "toggle")
       ;; Line toggles
       :desc "Comment or uncomment lines"     "/" #'comment-line
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       ;; Room toggles
       :desc "Mixed pitch"                    "a" #'mixed-pitch-mode
       :desc "Visual fill column"             "v" #'visual-fill-column-mode
       ))

(map! :localleader
      :map org-mode-map
      :desc "Org babel tangle" "TAB" #'org-babel-tangle)

(map! :leader
       :prefix ("n" . "notes")
       :desc "Dump brain"         "b" #'elk/org-roam-capture-inbox
       :desc "Find node"          "f" #'org-roam-node-find
       :desc "Insert node"        "i" #'org-roam-node-insert-immediate
       :desc "Insert node cap."   "I" #'org-roam-node-insert
       :desc "Capture to node"    "n" #'org-roam-capture
       :desc "Paste attach"       "p" #'elk/org-download-paste-clipboard
       :desc "Toggle roam buffer" "r" #'org-roam-buffer-toggle
       :desc "Task to prog."      "t" #'elk/org-roam-capture-task
       :desc "Web graph"          "w" #'org-roam-ui-mode
       (:prefix-map ("d" . "dailies")
       "-" #'org-roam-dailies-find-directory
       "d" #'org-roam-dailies-goto-date
       "y" #'org-roam-dailies-goto-yesterday
       "m" #'org-roam-dailies-goto-tomorrow
       "n" #'org-roam-dailies-goto-today

       "D" #'org-roam-dailies-capture-date
       "Y" #'org-roam-dailies-capture-yesterday
       "M" #'org-roam-dailies-capture-tomorrow
       "t" #'org-roam-dailies-capture-today
       ))

(after! deft
  (setq deft-directory org-directory
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t
        deft-recursive t
        deft-extensions '("md" "org")))

;; stop copying each visual state move to the clipboard:
;; https://github.com/emacs-evil/evil/issues/336
;; grokked from:
;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
(advice-add #'evil-visual-update-x-selection :override #'ignore)

(defun elk/org-initial-setup ()
  (locally-defer-font-lock)
  (visual-fill-column-mode +1))

(add-hook! 'org-mode-hook 'elk/org-initial-setup)

(custom-theme-set-faces
 'user
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-block ((t (:foreground nil))))
 '(org-tag ((t (:inherit org-tag :italic t))))
 '(org-ellipsis ((t (:inherit shadow :height 0.8))))
 '(org-link ((t (:foreground "royal blue" :underline t)))))

(after! org
  (setq org-ellipsis " ⬎ ") ;; ▼
  (setq org-highlight-latex-and-related '(native))) ;; Highlight inline LaTeX

(after! org
  (setq org-src-ask-before-returning-to-edit-buffer nil)

  ;; I want docx document for MS Word compatibility
  (setq org-odt-preferred-output-format "docx"))

(use-package! org-appear
    :commands (org-appear-mode)
    :hook (org-mode . org-appear-mode)
    :init
    (setq org-hide-emphasis-markers t) ;; A default setting that needs to be t for org-appear
    (setq org-appear-autoemphasis t)  ;; Enable org-appear on emphasis (bold, italics, etc)
    (setq org-appear-autolinks nil) ;; Enable on links
    (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript

(use-package! org-modern
  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package! org-modern-indent
  :hook
  (org-mode . org-modern-indent-mode))

(use-package! org-super-agenda
  :commands org-super-agenda-mode
  :config
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-header-prefix " ◦ "))

(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("n" . "notes"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package! org-roam
  :init
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-v2-ack t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (setq org-roam-completion-everywhere t)
  (setq org-roam-list-files-commands '(fd fdfind rg find))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun elk/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "braindump/inbox.org" "#+title: Inbox\n")))))

(defun elk/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun elk/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (elk/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun elk/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (elk/org-roam-list-notes-by-tag "Project")))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat (propertize "${type:10}" 'face 'org-tag) "${title:*} " (propertize "${tags:10}" 'face 'font-lock-comment-face)))

(setq org-roam-capture-templates
      '(("b" "brain" plain "\n%?"
         :if-new (file+head "brain/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "\n%?"
         :if-new (file+head "reference/${slug}.org" "#+title: ${title}\n#+date: %U\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "\n%?"
         :if-new (file+head "article/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: Seedling\n")
         :immediate-finish t
         :unnarrowed t)
        ("s" "school" plain "\n%?"
         :if-new (file+head "school/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
         :immediate-finish t
         :unnarrowed t)
        ("p" "project" plain "\n* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
         :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+date: %U\n#+filetags: Project\n\n")
         :immediate-finish t
         :unnarrowed t)
        ("t" "tag" plain "%?"
         :if-new (file+head "tag/${slug}.org" "#+title: ${title}\n\n")
         :immediate-finish t
         :unnarrowed t)
        ))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?"
         :if-new (file+head "journal/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d> Journal\n\n")
         :unnarrowed t)))

(defun elk/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'elk/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun elk/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'elk/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (elk/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+date: %U\n#+filetags: Project\n\n"("Tasks"))
                                   ))))

(defun elk/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun elk/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (elk/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun elk/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (elk/org-roam-list-notes-by-tag "projects")))

(org-roam-db-autosync-mode)
(elk/org-roam-refresh-agenda-list) ;; Build the agenda list the first time for the session
) ;; End of org-roam block

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package! kbd-mode
  :mode ("\\.kbd\\'" . kbd-mode))

(use-package! centered-cursor-mode
  :defer 0
  :diminish centered-cursor-mode
  :config
  (global-centered-cursor-mode))

(setq +format-with-lsp nil)

;; Lua
(set-formatter! 'stylua "stylua -" :modes '(lua-mode))
;; Haskell
(set-formatter! 'brittany "brittany" :modes '(haskell-mode))
;; Python
(set-formatter! 'autopep8 "autopep8 -" :modes '(python-mode))

(set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}"))

(use-package! platformio-mode
  :config
  ;; Enable ccls for all c++ files, and platformio-mode only
  ;; when needed (platformio.ini present in project root).
  (add-hook 'c++-mode-hook (lambda ()
                             (lsp-deferred)
                             (platformio-conditionally-enable))))
