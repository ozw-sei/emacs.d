(use-package vterm
  :straight t
  :config
  ;; Bottom terminal configuration
  (defvar my/vterm-bottom-buffer-name "*vterm-bottom*"
    "Buffer name for bottom terminal.")

  (defvar my/vterm-bottom-height 20
    "Height of bottom terminal window.")

  (defun my/vterm-toggle-bottom ()
    "Toggle vterm at bottom of frame like VSCode."
    (interactive)
    (let ((vterm-buffer (get-buffer my/vterm-bottom-buffer-name))
          (vterm-window (get-buffer-window my/vterm-bottom-buffer-name)))
      (cond
       ;; If window is visible, hide it
       (vterm-window
        (delete-window vterm-window))
       ;; If buffer exists but window is not visible, show it
       (vterm-buffer
        (display-buffer-at-bottom vterm-buffer
                                  `((window-height . ,my/vterm-bottom-height))))
       ;; If buffer doesn't exist, create and show it
       (t
        (let ((buffer (generate-new-buffer my/vterm-bottom-buffer-name)))
          (with-current-buffer buffer
            (vterm-mode))
          (display-buffer-at-bottom buffer
                                    `((window-height . ,my/vterm-bottom-height))))))))

  (defun my/vterm-project-toggle-bottom ()
    "Toggle vterm at bottom with project root as working directory."
    (interactive)
    (let* ((project-root (if (and (fboundp 'projectile-project-p)
                                  (projectile-project-p))
                             (projectile-project-root)
                           default-directory))
           (buffer-name (format "*vterm-bottom-%s*"
                               (file-name-nondirectory
                                (directory-file-name project-root))))
           (vterm-buffer (get-buffer buffer-name))
           (vterm-window (get-buffer-window buffer-name)))
      (cond
       ;; If window is visible, hide it
       (vterm-window
        (delete-window vterm-window))
       ;; If buffer exists but window is not visible, show it
       (vterm-buffer
        (display-buffer-at-bottom vterm-buffer
                                  `((window-height . ,my/vterm-bottom-height))))
       ;; If buffer doesn't exist, create and show it
       (t
        (let ((buffer (generate-new-buffer buffer-name)))
          (with-current-buffer buffer
            (let ((default-directory project-root))
              (vterm-mode)))
          (display-buffer-at-bottom buffer
                                    `((window-height . ,my/vterm-bottom-height))))))))

  ;; Custom display function for bottom terminal
  (defun my/display-vterm-bottom (buffer alist)
    "Display vterm buffer at bottom of frame."
    (let ((window (split-window (frame-root-window)
                               (- my/vterm-bottom-height) 'below)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      window))

  ;; Add to display-buffer-alist for consistent behavior
  (add-to-list 'display-buffer-alist
               `(,(rx "*vterm-bottom" (zero-or-more anything) "*")
                 (display-buffer-at-bottom)
                 (window-height . ,my/vterm-bottom-height)
                 (dedicated . t)
                 (preserve-size . (nil . t))))

  ;; Prevent vterm buffers from being selected when switching buffers
  (defun my/vterm-buffer-p (buffer)
    "Return t if BUFFER is a vterm buffer."
    (string-match-p "\\*vterm-bottom.*\\*" (buffer-name buffer)))

  ;; Optional: Auto-hide on focus loss (like VSCode)
  (defun my/vterm-hide-on-focus-out ()
    "Hide vterm bottom window when focus leaves it."
    (when (and (my/vterm-buffer-p (current-buffer))
               (not (eq (selected-window) (get-buffer-window (current-buffer)))))
      (let ((window (get-buffer-window (current-buffer))))
        (when window
          (delete-window window)))))

  ;; Keybindings
  (global-set-key (kbd "M-j") 'my/vterm-toggle-bottom)
  (global-set-key (kbd "C-~") 'my/vterm-project-toggle-bottom))
