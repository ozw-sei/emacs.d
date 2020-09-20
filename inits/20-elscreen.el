
;; elscreen
;; (use-package elscreen
;;   :straight t
;;   :init
;;   (elscreen-start))

;; hydra window 操作
(defhydra hydra-buffer-split nil
  "hydra-buffer-split"
  ("s" (lambda ()
        (interactive)
        (split-window-vertically)
        (windmove-down)) "split-horizontally")

  ("v" (lambda()
        (interactive)
        (split-window-horizontally)
        (windmove-right)) "split-vertically")
  ("C-k" delete-window "delete")
  ("h" windmove-left "move-left")
  ("l" windmove-right "move-right")
  ("k" windmove-up "move-up")
  ("j" windmove-down "move-down")
  ;; ("c" elscreen-create "screen-create")
  ;; ("n" elscreen-next "screen-next")
  ;; ("p" elscreen-previous "screen-prev")
  ;; ("x" elscreen-kill "screen-kill")
  )



;; (use-package zoom-window
;;   :straight t
;;   :custom
;;   (zoom-window-use-elscreen t)
;;   :config
;;   (zoom-window-setup)
;;   )
;; (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
