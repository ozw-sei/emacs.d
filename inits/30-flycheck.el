;; Flycheck - lsp-mode uses flycheck by default for diagnostics
(use-package flycheck
  :straight t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; flycheck is auto-enabled by lsp-mode for LSP languages
  ;; Enable explicitly for non-LSP modes
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (add-hook 'shell-script-mode-hook #'flycheck-mode))

(use-package flycheck-color-mode-line
  :straight t
  :config
  (flycheck-color-mode-line-mode 1))

(use-package flycheck-pos-tip
  :straight t
  :config
  (flycheck-pos-tip-mode 1))

(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

;; Claude Code エラー連携機能 (hydra より先に定義)
(defun claude-code--get-current-error ()
  "Get current error information from flycheck"
  (when (and (bound-and-true-p flycheck-mode)
             (flycheck-overlay-errors-at (point)))
    (let ((err (car (flycheck-overlay-errors-at (point)))))
      (list :message (flycheck-error-message err)
            :file (buffer-file-name)
            :line (flycheck-error-line err)
            :type (flycheck-error-level err)))))

(defun claude-code--get-error-context ()
  "Get surrounding context for the error"
  (save-excursion
    (let ((start (max (point-min) (- (line-beginning-position) 300)))
          (end (min (point-max) (+ (line-end-position) 300))))
      (buffer-substring-no-properties start end))))

(defun claude-code--send-to-claude (message)
  "Send message to Claude Code via kill-ring"
  (kill-new message)
  (message "Claude Code用のメッセージをクリップボードにコピーしました。claude-codeで貼り付けてください。"))

(defun claude-code-explain-error ()
  "Send current error to Claude Code for explanation"
  (interactive)
  (let ((error-info (claude-code--get-current-error)))
    (when error-info
      (claude-code--send-to-claude
       (format "このエラーを説明してください:\n\n%s\n\nファイル: %s\n行: %d"
               (plist-get error-info :message)
               (plist-get error-info :file)
               (plist-get error-info :line))))))

(defun claude-code-fix-error ()
  "Ask Claude Code to fix current error"
  (interactive)
  (let ((error-info (claude-code--get-current-error))
        (context (claude-code--get-error-context)))
    (when error-info
      (claude-code--send-to-claude
       (format "このエラーを修正してください:\n\n%s\n\nファイル: %s\n行: %d\n\nコンテキスト:\n%s"
               (plist-get error-info :message)
               (plist-get error-info :file)
               (plist-get error-info :line)
               context)))))

;; hydra flycheck 操作
(defhydra hydra-flycheck nil
  "hydra-flycheck"
  ("j" flycheck-next-error     "next-error")
  ("k" flycheck-previous-error "prev-error")
  ("h" flycheck-first-error    "first-error")
  ("l" (lambda ()
        (interactive)
        (flycheck-list-errors)
        (windmove-right)) "list-errors" :exit t)
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("e" claude-code-explain-error "explain-error" :exit t)
  ("f" claude-code-fix-error "fix-error" :exit t)
  ("q" nil "exit" :color blue))

(bind-key "C-;" 'hydra-flycheck/body)
