
;; (use-package real-auto-save
;;   :straight t
;;   :diminish (real-auto-save-mode)
;;   :config
;;   (setq real-auto-save-interval 10)        ;10秒後に自動保存
;;   (add-hook 'find-file-hook 'real-auto-save-mode))


(use-package super-save
  :straight t
  :diminish (super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-exclude '(".gpg"))

  :config
  (super-save-mode +1))
