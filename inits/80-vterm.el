(use-package vterm
  :straight t
  :config
  ;; Bottom terminal panel configuration (VSCode-like)
  (defvar my/vterm-bottom-height 15
    "Default height of bottom terminal (number of lines).")

  (defvar my/vterm-bottom-maximized nil
    "Whether the bottom terminal is currently maximized.")

  (defvar my/vterm-bottom-saved-window-config nil
    "Saved window configuration before maximizing terminal.")

  ;; --- Core: find or create the vterm buffer for current project ---
  (defun my/vterm-bottom-buffer-name ()
    "Return the vterm buffer name for the current project."
    (let ((project-root (if (and (fboundp 'projectile-project-p)
                                 (projectile-project-p))
                            (projectile-project-root)
                          default-directory)))
      (format "*vterm-%s*"
              (file-name-nondirectory
               (directory-file-name project-root)))))

  (defun my/vterm-bottom-get-or-create ()
    "Get existing vterm buffer or create a new one for the project."
    (let* ((buf-name (my/vterm-bottom-buffer-name))
           (buf (get-buffer buf-name)))
      (or buf
          (let ((project-root (if (and (fboundp 'projectile-project-p)
                                       (projectile-project-p))
                                  (projectile-project-root)
                                default-directory)))
            (let ((buffer (generate-new-buffer buf-name)))
              (with-current-buffer buffer
                (let ((default-directory project-root))
                  (vterm-mode)))
              buffer)))))

  (defun my/vterm-bottom-window ()
    "Return the vterm bottom window if visible."
    (let ((buf (get-buffer (my/vterm-bottom-buffer-name))))
      (when buf (get-buffer-window buf))))

  ;; --- Toggle: show/hide ---
  (defun my/vterm-toggle ()
    "Toggle bottom terminal panel. Creates one if it doesn't exist."
    (interactive)
    (let ((win (my/vterm-bottom-window)))
      (cond
       ;; Maximized -> restore
       (my/vterm-bottom-maximized
        (my/vterm-restore))
       ;; Visible -> hide
       (win
        (delete-window win))
       ;; Hidden -> show
       (t
        (let ((buf (my/vterm-bottom-get-or-create)))
          (display-buffer-at-bottom buf
                                   `((window-height . ,my/vterm-bottom-height)
                                     (dedicated . t)
                                     (preserve-size . (nil . t))))
          (select-window (get-buffer-window buf)))))))

  ;; --- Maximize / Restore ---
  (defun my/vterm-maximize ()
    "Maximize the terminal panel to full frame."
    (interactive)
    (let ((buf (my/vterm-bottom-get-or-create)))
      (setq my/vterm-bottom-saved-window-config (current-window-configuration))
      (setq my/vterm-bottom-maximized t)
      (delete-other-windows)
      (switch-to-buffer buf)))

  (defun my/vterm-restore ()
    "Restore window layout from maximized terminal."
    (interactive)
    (when my/vterm-bottom-saved-window-config
      (set-window-configuration my/vterm-bottom-saved-window-config)
      (setq my/vterm-bottom-saved-window-config nil))
    (setq my/vterm-bottom-maximized nil))

  (defun my/vterm-toggle-maximize ()
    "Toggle between maximized and normal terminal panel size."
    (interactive)
    (if my/vterm-bottom-maximized
        (my/vterm-restore)
      (my/vterm-maximize)))

  ;; --- Resize ---
  (defun my/vterm-resize-up ()
    "Make the terminal panel taller."
    (interactive)
    (let ((win (my/vterm-bottom-window)))
      (when win
        (with-selected-window win
          (enlarge-window 5)))))

  (defun my/vterm-resize-down ()
    "Make the terminal panel shorter."
    (interactive)
    (let ((win (my/vterm-bottom-window)))
      (when win
        (with-selected-window win
          (shrink-window 5)))))

  ;; --- New terminal tab ---
  (defun my/vterm-new ()
    "Open a new vterm buffer in the bottom panel."
    (interactive)
    (let* ((project-root (if (and (fboundp 'projectile-project-p)
                                   (projectile-project-p))
                              (projectile-project-root)
                            default-directory))
           (buf-name (generate-new-buffer-name
                      (format "*vterm-%s*"
                              (file-name-nondirectory
                               (directory-file-name project-root)))))
           (buffer (generate-new-buffer buf-name)))
      (with-current-buffer buffer
        (let ((default-directory project-root))
          (vterm-mode)))
      (let ((win (my/vterm-bottom-window)))
        (if win
            (set-window-buffer win buffer)
          (display-buffer-at-bottom buffer
                                   `((window-height . ,my/vterm-bottom-height)
                                     (dedicated . t))))
        (select-window (get-buffer-window buffer)))))

  ;; --- Switch between terminal buffers ---
  (defun my/vterm-switch ()
    "Switch between open vterm buffers."
    (interactive)
    (let ((vterm-buffers (cl-remove-if-not
                          (lambda (b) (with-current-buffer b
                                        (derived-mode-p 'vterm-mode)))
                          (buffer-list))))
      (if (null vterm-buffers)
          (message "No vterm buffers")
        (let* ((names (mapcar #'buffer-name vterm-buffers))
               (selected (completing-read "Terminal: " names nil t)))
          (let ((win (my/vterm-bottom-window)))
            (if win
                (progn (set-window-buffer win (get-buffer selected))
                       (select-window win))
              (display-buffer-at-bottom (get-buffer selected)
                                       `((window-height . ,my/vterm-bottom-height)
                                         (dedicated . t)))
              (select-window (get-buffer-window (get-buffer selected)))))))))

  ;; --- Focus toggle: jump between editor and terminal ---
  (defun my/vterm-focus-toggle ()
    "Toggle focus between editor and terminal panel."
    (interactive)
    (let ((win (my/vterm-bottom-window)))
      (cond
       ;; In terminal -> go back to previous window
       ((and win (eq (selected-window) win))
        (other-window 1))
       ;; Not in terminal but it's visible -> focus it
       (win
        (select-window win))
       ;; Not visible -> open it
       (t
        (my/vterm-toggle)))))

  ;; --- display-buffer-alist for consistent placement ---
  (add-to-list 'display-buffer-alist
               `("\\*vterm-.*\\*"
                 (display-buffer-at-bottom)
                 (window-height . ,my/vterm-bottom-height)
                 (dedicated . t)
                 (preserve-size . (nil . t))))

  ;; --- Hydra for terminal operations ---
  (defhydra hydra-terminal (:hint nil :exit t)
    "
 Terminal Panel
 ^^────────────────────────────
 [_t_] toggle    [_m_] maximize/restore
 [_n_] new       [_s_] switch
 [_f_] focus     [_k_] kill
 ^^────────────────────────────
 [_+_] taller    [_-_] shorter
"
    ("t" my/vterm-toggle)
    ("m" my/vterm-toggle-maximize)
    ("n" my/vterm-new)
    ("s" my/vterm-switch)
    ("f" my/vterm-focus-toggle)
    ("k" (lambda () (interactive)
           (let ((buf (get-buffer (my/vterm-bottom-buffer-name))))
             (when buf
               (let ((win (get-buffer-window buf)))
                 (when win (delete-window win)))
               (kill-buffer buf)))) :exit t)
    ("+" my/vterm-resize-up :exit nil)
    ("-" my/vterm-resize-down :exit nil)
    ("q" nil "quit"))

  ;; --- Global keybindings ---
  ;; M-j: toggle terminal (like VSCode Ctrl+`)
  (global-set-key (kbd "M-j") 'my/vterm-focus-toggle)
  ;; C-`: project terminal toggle
  (global-set-key (kbd "C-`") 'my/vterm-toggle)
  ;; C-M-j: terminal hydra for all operations
  (global-set-key (kbd "C-M-j") 'hydra-terminal/body))
