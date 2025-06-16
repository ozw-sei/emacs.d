;; Disable flycheck globally (using Flymake with LSP instead)
;; Keep flycheck for non-LSP modes if needed
(use-package flycheck
  :straight t
  :config
  ;; Don't enable globally - let Flymake handle LSP diagnostics
  ;; (global-flycheck-mode 1)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  
  ;; Only enable flycheck for specific modes where Flymake isn't sufficient
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

;; hydra flcheck 操作
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
  ("q" nil "exit" :color blue))

(bind-key "C-;" 'hydra-flycheck/body)

;; Claude Code連携機能
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

(defun claude-code--get-current-error ()
  "Get current error information from flymake or flycheck"
  (cond
   ;; flymake
   ((and (bound-and-true-p flymake-mode)
         (flymake-diagnostics (point)))
    (let ((diag (car (flymake-diagnostics (point)))))
      (list :message (flymake-diagnostic-text diag)
            :file (buffer-file-name)
            :line (line-number-at-pos)
            :type (flymake-diagnostic-type diag))))
   ;; flycheck
   ((and (bound-and-true-p flycheck-mode)
         (flycheck-overlay-errors-at (point)))
    (let ((err (car (flycheck-overlay-errors-at (point)))))
      (list :message (flycheck-error-message err)
            :file (buffer-file-name)
            :line (flycheck-error-line err)
            :type (flycheck-error-level err))))
   (t nil)))

(defun claude-code--get-error-context ()
  "Get surrounding context for the error"
  (save-excursion
    (let ((start (max (point-min) (- (line-beginning-position) 300)))
          (end (min (point-max) (+ (line-end-position) 300))))
      (buffer-substring-no-properties start end))))

(defun claude-code--send-to-claude (message)
  "Send message to Claude Code (placeholder for actual implementation)"
  ;; 実際の実装では、claude-codeのAPIやCLIを呼び出す
  ;; 今は一時的にkill-ringにコピーしてユーザーに通知
  (kill-new message)
  (message "Claude Code用のメッセージをクリップボードにコピーしました。claude-codeで貼り付けてください。"))

;; hydra flymake操作
(defhydra hydra-flymake nil
  "hydra-flymake"
  ("j" flymake-goto-next-error     "next-error")
  ("k" flymake-goto-prev-error     "prev-error")
  ("l" flymake-show-diagnostics-buffer "show-diagnostics" :exit t)
  ("L" flymake-show-project-diagnostics "project-diagnostics" :exit t)
  ("c" flymake-start "check/restart")
  ("r" flymake-running-backends "running-backends")
  ("d" flymake-disabled-backends "disabled-backends")
  ("e" claude-code-explain-error "explain-error" :exit t)
  ("f" claude-code-fix-error "fix-error" :exit t)
  ("q" nil "exit" :color blue))

(bind-key "C-:" 'hydra-flymake/body)
