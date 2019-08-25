;;; このときidoが使うwindowの高さは大きくした方がいい
(setq ido-max-window-height 0.75)
;;; あいまいマッチは入れておこう
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-vertical-mode 1)
;;; [2015-07-07 Tue]new: C-n/C-pで選択
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;; 他の選択肢: ↑と↓でも選択できるようにする
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
;;; ←と→で履歴も辿れるようにする
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


