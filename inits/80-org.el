(setq org-directory (expand-file-name "$HOME/org"))
(setq my-org-agenda-dir "$HOME/org/")

(setq org-default-notes-file (concat org-directory "/mygtd.org"))

(setq org-todo-keywords
      '(
        (sequence "IDEA(i)" "TODO(t)" "DOING(d)""WAITING(w)" "|" "DONE")
        (sequence "|" "CANCELED(c)" "SOMEDAY(f)")
        ))
(setq org-startup-with-inline-images t)
(setq org-clock-into-drawer t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-log-done 'time)


;; ショートカットキー
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(use-package org-bullets
  :straight t
  :config
  (org-bullets-mode 1))

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("e" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))


(define-key org-mode-map "<"
  (lambda () (interactive)
     (if (looking-back "^")
         (hydra-org-template/body)
       (self-insert-command 1))))
