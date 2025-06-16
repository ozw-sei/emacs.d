;; Modern Python development with pylsp and uv integration
(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . eglot-ensure))
  :config
  ;; Configure pylsp for comprehensive Python support
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))
  
  ;; pylsp configuration for enhanced development
  (setq-default eglot-workspace-configuration
                (append eglot-workspace-configuration
                        '((:pylsp . 
                           (:configurationSources ["flake8"]
                            :plugins 
                            (:pycodestyle (:enabled nil)
                             :mccabe (:enabled nil)
                             :flake8 (:enabled t)
                             :black (:enabled t)
                             :isort (:enabled t)
                             :rope_completion (:enabled t)
                             :rope_autoimport (:enabled t)
                             :pylsp_mypy (:enabled t :live_mode nil)))))))
  
  ;; UV runtime detection and integration  
  (defun python-detect-uv-project ()
    "Detect if current project uses uv and configure accordingly"
    (when-let ((project-root (projectile-project-root)))
      (or (file-exists-p (expand-file-name "pyproject.toml" project-root))
          (file-exists-p (expand-file-name "uv.lock" project-root)))))
  
  (defun python-setup-uv-environment ()
    "Setup Python environment with uv if available"
    (when (and (python-detect-uv-project)
               (executable-find "uv"))
      (let* ((project-root (projectile-project-root))
             (venv-path (expand-file-name ".venv" project-root))
             (python-executable (expand-file-name "bin/python" venv-path)))
        (when (file-exists-p python-executable)
          (setq-local python-shell-interpreter python-executable)
          (setq-local eglot-python-executable python-executable)))))
  
  (add-hook 'python-mode-hook #'python-setup-uv-environment)
  
  ;; UV task runner integration
  (defun python-uv-run-task (task)
    "Run uv task in current project"
    (interactive (list (read-string "UV task: " "test")))
    (if (python-detect-uv-project)
        (let ((default-directory (projectile-project-root)))
          (compile (format "uv run %s" task)))
      (error "Not in a uv project")))
  
  (defun python-uv-sync ()
    "Sync uv dependencies"
    (interactive)
    (when (python-detect-uv-project)
      (let ((default-directory (projectile-project-root)))
        (compile "uv sync"))))
  
  ;; Key bindings for uv integration
  (define-key python-mode-map (kbd "C-c C-u t") 'python-uv-run-task)
  (define-key python-mode-map (kbd "C-c C-u s") 'python-uv-sync)
  
  ;; Unset conflicting keybinding
  (local-unset-key (kbd "C-c C-p")))

;; Poetry integration (fallback for non-uv projects)
(use-package poetry
  :straight t
  :hook (python-mode . poetry-tracking-mode)
  :config
  (defun poetry-activate-if-available ()
    "Activate poetry environment if not using uv"
    (unless (python-detect-uv-project)
      (when (poetry-find-project-root)
        (poetry-venv-activate))))
  
  (add-hook 'python-mode-hook #'poetry-activate-if-available))

;; Enhanced testing support
(use-package python-pytest
  :straight t
  :hook (python-mode . python-pytest-mode)
  :custom
  (python-pytest-executable "python -m pytest")
  :config
  ;; UV-aware pytest execution
  (defun python-pytest-run-uv (args)
    "Run pytest through uv if available"
    (if (python-detect-uv-project)
        (python-pytest-run (concat "uv run " args))
      (python-pytest-run args)))
  
  (define-key python-pytest-mode-map (kbd "C-c C-t u") 'python-pytest-run-uv))

;; Black formatting integration (handled by pylsp)
(use-package blacken
  :straight t
  :hook (python-ts-mode . blacken-mode)
  :custom
  (blacken-fast-unsafe t)
  (blacken-line-length 88))

;; Python debugging with pdb integration
;; ===============================================
;; 
;; pdb (Python Debugger) 完全活用ガイド
;; =====================================
;;
;; ## pdbとは
;; Pythonの標準デバッガー。対話的にコードをステップ実行し、変数の値を確認したり
;; 任意のPythonコードを実行できる。gdbのPython版と考えると分かりやすい。
;;
;; ## 基本的な使い方
;;
;; ### 1. ブレークポイントの設定方法
;; 
;; #### 方法A: コードに直接埋め込み (推奨)
;; ```python
;; import pdb; pdb.set_trace()  # この行で実行が停止する
;; ```
;; - Emacs: C-c C-b で自動挿入
;; - 最も確実で、どの環境でも動作する
;;
;; #### 方法B: Python 3.7以降の breakpoint()
;; ```python
;; breakpoint()  # Python 3.7+ のみ
;; ```
;; - 環境変数PYTHONBREAKPOINTで無効化可能
;;
;; #### 方法C: コマンドラインから起動
;; ```bash
;; python -m pdb script.py  # スクリプト全体をデバッグモードで実行
;; ```
;; - Emacs: C-c C-d で実行
;;
;; ### 2. pdbコマンド一覧 (デバッガー内で使用)
;;
;; #### 実行制御
;; - n (next)      : 次の行を実行 (関数呼び出しはステップオーバー)
;; - s (step)      : 次の行を実行 (関数内部にステップイン)
;; - c (continue)  : 次のブレークポイントまで実行継続
;; - r (return)    : 現在の関数から戻るまで実行
;; - q (quit)      : デバッガーを終了
;;
;; #### 位置確認
;; - w (where)     : 現在のスタックトレースを表示
;; - l (list)      : 現在位置の周辺コードを表示
;; - ll (longlist) : 現在の関数全体を表示
;; - u (up)        : スタックを上に移動
;; - d (down)      : スタックを下に移動
;;
;; #### 変数・オブジェクト確認
;; - p <変数名>    : 変数の値を表示
;; - pp <変数名>   : 変数の値を整形して表示
;; - a (args)      : 現在の関数の引数を表示
;; - locals()      : ローカル変数一覧
;; - globals()     : グローバル変数一覧
;; - type(変数名)  : 変数の型を確認
;; - dir(オブジェクト) : オブジェクトの属性・メソッド一覧
;;
;; #### ブレークポイント管理
;; - b <行番号>           : 指定行にブレークポイント設定
;; - b <ファイル名>:<行番号> : 他ファイルの指定行に設定
;; - b <関数名>           : 関数の開始時に設定
;; - cl (clear)           : 全ブレークポイント削除
;; - cl <番号>            : 指定番号のブレークポイント削除
;; - disable <番号>       : ブレークポイントを無効化
;; - enable <番号>        : ブレークポイントを有効化
;; - bl (break list)      : ブレークポイント一覧表示
;;
;; #### 条件付きブレークポイント
;; ```
;; b 10, variable > 5     # 変数が5より大きい時のみ停止
;; ```
;;
;; ### 3. 実践的なデバッグテクニック
;;
;; #### 変数の動的変更
;; ```
;; (Pdb) variable_name = new_value  # 変数の値を変更
;; (Pdb) del variable_name          # 変数を削除
;; ```
;;
;; #### 任意のコードの実行
;; ```
;; (Pdb) print([x for x in range(10) if x % 2 == 0])  # リスト内包表記
;; (Pdb) import math; print(math.sqrt(16))             # モジュールインポート
;; ```
;;
;; #### 複数行コードの実行
;; ```
;; (Pdb) !for i in range(3):
;; (Pdb) !    print(f"Value {i}: {some_list[i]}")
;; ```
;; - 行頭に ! を付けることで複数行のPythonコードが実行可能
;;
;; #### 関数・メソッドの動的定義
;; ```
;; (Pdb) def debug_helper(obj):
;; (Pdb) ...     return [attr for attr in dir(obj) if not attr.startswith('_')]
;; (Pdb) debug_helper(my_object)
;; ```
;;
;; ### 4. 高度な使用法
;;
;; #### エイリアスの設定
;; ```
;; (Pdb) alias ll longlist                    # llをlonglistの短縮形に
;; (Pdb) alias pd pp %1.__dict__              # オブジェクトの辞書表示
;; ```
;;
;; #### .pdbrcファイル（ホームディレクトリに配置）
;; ```
;; # ~/.pdbrc
;; alias ll longlist
;; alias pd pp %1.__dict__
;; alias pv pp vars()
;; ```
;;
;; #### post-mortem デバッグ（例外発生時の自動デバッグ）
;; ```python
;; import pdb
;; import sys
;; 
;; def excepthook(type, value, tb):
;;     pdb.post_mortem(tb)
;; 
;; sys.excepthook = excepthook  # 例外発生時に自動でpdb起動
;; ```
;;
;; ### 5. Emacsでの操作フロー例
;;
;; 1. C-c C-b でブレークポイント設定
;; 2. C-c C-c でスクリプト実行 (または C-c C-d でpdb実行)
;; 3. *compilation* バッファでpdbコマンド入力
;; 4. n, s, c などでステップ実行
;; 5. p variable_name で変数確認
;; 6. C-c C-r で不要なブレークポイント削除
;;
;; ### 6. トラブルシューティング
;;
;; #### よくある問題と解決法
;; - "BdbQuit" エラー: q コマンドで正常終了させる
;; - ブレークポイントが効かない: インデントが正しいか確認
;; - 変数が見えない: スコープを w コマンドで確認
;; - 無限ループ: Ctrl+C で強制停止後、q で終了
;;
;; #### デバッグ効率化のコツ
;; - 問題の範囲を絞ってからブレークポイント設定
;; - pp コマンドで複雑なデータ構造も見やすく表示
;; - 条件付きブレークポイントで特定の状況のみ停止
;; - l コマンドで現在のコンテキストを常に確認
;;
(use-package python
  :config
  ;; Enhanced pdb integration
  (defvar python-pdb-buffer-name "*Python-pdb*"
    "Name of the pdb buffer.")
  
  (defun python-pdb-set-breakpoint ()
    "Set a pdb breakpoint at current line.
    
pdb.set_trace()を現在行に挿入します。実行時にこの行で停止し、
対話的デバッグモードに入ります。

使用例:
1. デバッグしたい行にカーソルを置く
2. C-c C-b を押す  
3. スクリプトを実行すると、この行で停止してpdbモードに入る"
    (interactive)
    (let ((line-number (line-number-at-pos)))
      (save-excursion
        (beginning-of-line)
        (insert (format "import pdb; pdb.set_trace()  # Breakpoint at line %d\n" line-number)))
      (message "Breakpoint set at line %d" line-number)))
  
  (defun python-pdb-remove-breakpoints ()
    "Remove all pdb breakpoints from current buffer.
    
ファイル内のすべての pdb.set_trace() 行を削除します。
デバッグ完了後のクリーンアップに使用します。"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ".*import pdb; pdb\\.set_trace().*\n" nil t)
        (replace-match ""))
      (message "All pdb breakpoints removed")))
  
  (defun python-pdb-run-current-file ()
    "Run current Python file with pdb.
    
現在のファイルをpdbモードで実行します。
スクリプトの最初からステップ実行が可能です。

実行後のpdbコマンド例:
- n: 次の行を実行
- s: 関数内部にステップイン
- c: 次のブレークポイントまで継続
- l: 現在位置の周辺コードを表示
- p variable_name: 変数の値を表示"
    (interactive)
    (when (buffer-file-name)
      (let ((file-name (buffer-file-name)))
        (compile (format "python -m pdb %s" file-name)))))
  
  ;; Modern debugging with debugpy
  (defun python-debugpy-set-breakpoint ()
    "Set a debugpy breakpoint at current line (VS Code compatible)."
    (interactive)
    (let ((line-number (line-number-at-pos)))
      (save-excursion
        (beginning-of-line)
        (insert (format "import debugpy; debugpy.breakpoint()  # Breakpoint at line %d\n" line-number)))
      (message "debugpy breakpoint set at line %d" line-number)))
  
  (defun python-debugpy-listen ()
    "Start debugpy server for remote debugging."
    (interactive)
    (let ((port (read-string "Debug port (default 5678): " "5678")))
      (save-excursion
        (goto-char (point-min))
        (insert (format "import debugpy\ndebugpy.listen(%s)\ndebugpy.wait_for_client()\n\n" port)))
      (message "debugpy server setup added (port %s)" port)))
  
  ;; Enhanced REPL integration
  (defun python-send-region-to-repl (start end)
    "Send region to Python REPL."
    (interactive "r")
    (when (use-region-p)
      (python-shell-send-region start end)
      (python-shell-switch-to-shell)))
  
  (defun python-send-buffer-to-repl ()
    "Send entire buffer to Python REPL."
    (interactive)
    (python-shell-send-buffer)
    (python-shell-switch-to-shell))
  
  (defun python-send-defun-to-repl ()
    "Send current function to Python REPL."
    (interactive)
    (python-shell-send-defun)
    (python-shell-switch-to-shell))
  
  ;; Key bindings for debugging and REPL
  (define-key python-ts-mode-map (kbd "C-c C-b") 'python-pdb-set-breakpoint)
  (define-key python-ts-mode-map (kbd "C-c C-r") 'python-pdb-remove-breakpoints)
  (define-key python-ts-mode-map (kbd "C-c C-d") 'python-pdb-run-current-file)
  (define-key python-ts-mode-map (kbd "C-c C-x b") 'python-debugpy-set-breakpoint)
  (define-key python-ts-mode-map (kbd "C-c C-x l") 'python-debugpy-listen)
  (define-key python-ts-mode-map (kbd "C-c C-e") 'python-send-region-to-repl)
  (define-key python-ts-mode-map (kbd "C-c C-s") 'python-send-buffer-to-repl)
  (define-key python-ts-mode-map (kbd "C-c C-f") 'python-send-defun-to-repl))

;; Enhanced Python REPL with IPython support
(with-eval-after-load 'python-ts-mode
  ;; IPython integration
  (defun python-setup-ipython ()
    "Setup IPython as Python shell if available."
    (when (executable-find "ipython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
  
  ;; Project-aware Python shell
  (defun python-shell-project ()
    "Start Python shell in project context."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (when project-root
        (let ((default-directory project-root))
          (python-shell-switch-to-shell)))))
  
  (add-hook 'python-ts-mode-hook #'python-setup-ipython)
  (define-key python-ts-mode-map (kbd "C-c C-p") 'python-shell-project))

;; Jupyter notebook integration
(use-package ein
  :straight t
  :commands (ein:run ein:login)
  :config
  (setq ein:output-area-inlined-images t)
  (setq ein:slice-image t))

;; Python virtual environment management
(use-package pyvenv
  :straight t
  :hook (python-ts-mode . pyvenv-mode)
  :config
  ;; Auto-activate virtual environment
  (defun pyvenv-auto-activate ()
    "Auto-activate virtual environment based on project."
    (let ((project-root (projectile-project-root)))
      (when project-root
        (cond
         ;; .venv directory
         ((file-exists-p (expand-file-name ".venv" project-root))
          (pyvenv-activate (expand-file-name ".venv" project-root)))
         ;; venv directory
         ((file-exists-p (expand-file-name "venv" project-root))
          (pyvenv-activate (expand-file-name "venv" project-root)))
         ;; Poetry environment
         ((and (file-exists-p (expand-file-name "pyproject.toml" project-root))
               (executable-find "poetry"))
          (let ((poetry-venv (string-trim (shell-command-to-string "poetry env info --path"))))
            (when (file-exists-p poetry-venv)
              (pyvenv-activate poetry-venv))))))))
  
  (add-hook 'python-ts-mode-hook #'pyvenv-auto-activate)
  
  (define-key python-ts-mode-map (kbd "C-c C-v a") 'pyvenv-activate)
  (define-key python-ts-mode-map (kbd "C-c C-v d") 'pyvenv-deactivate)
  (define-key python-ts-mode-map (kbd "C-c C-v w") 'pyvenv-workon))

;; Enhanced Python LSP setup documentation
(defun python-lsp-setup-guide ()
  "Display Python LSP setup guide."
  (interactive)
  (with-help-window "*Python LSP Setup*"
    (princ "Python LSP Setup Guide\n")
    (princ "======================\n\n")
    (princ "1. Install python-lsp-server (pylsp):\n")
    (princ "   pip install 'python-lsp-server[all]'\n\n")
    (princ "2. Install additional plugins:\n")
    (princ "   pip install python-lsp-black      # Black formatting\n")
    (princ "   pip install pylsp-mypy           # Type checking\n")
    (princ "   pip install python-lsp-isort     # Import sorting\n")
    (princ "   pip install pylsp-rope           # Refactoring\n\n")
    (princ "3. For uv projects:\n")
    (princ "   uv add --dev 'python-lsp-server[all]'\n")
    (princ "   uv add --dev python-lsp-black pylsp-mypy\n\n")
    (princ "4. For Poetry projects:\n")
    (princ "   poetry add --group dev 'python-lsp-server[all]'\n\n")
    (princ "5. Alternative LSP servers:\n")
    (princ "   - Pyright: npm install -g pyright\n")
    (princ "   - Jedi Language Server: pip install jedi-language-server\n\n")
    (princ "Key bindings:\n")
    (princ "=============\n")
    (princ "C-c C-b  : Set pdb breakpoint\n")
    (princ "C-c C-r  : Remove all breakpoints\n")
    (princ "C-c C-d  : Run with pdb\n")
    (princ "C-c C-x b: Set debugpy breakpoint\n")
    (princ "C-c C-x l: Setup debugpy server\n")
    (princ "C-c C-e  : Send region to REPL\n")
    (princ "C-c C-s  : Send buffer to REPL\n")
    (princ "C-c C-f  : Send function to REPL\n")
    (princ "C-c C-p  : Start project Python shell\n")
    (princ "C-c C-v a: Activate virtual environment\n")))

;; Add to Python mode menu
(define-key python-ts-mode-map (kbd "C-c C-h") 'python-lsp-setup-guide)
