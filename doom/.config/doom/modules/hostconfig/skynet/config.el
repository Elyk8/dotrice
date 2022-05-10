;;; hostconfig/skynet/config.el -*- lexical-binding: t; -*-

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
