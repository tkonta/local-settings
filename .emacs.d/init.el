;;; init.el --- my init.el
;;; Commentary:
;; 
;;; Code:
;;

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


(setq dired-use-ls-dired nil)

;; 日本語環境でのバックスラッシュ入力
(define-key global-map [?¥] [?\\])

;; leaf

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))


(leaf env
  :doc "環境設定"
  :config
  (leaf leaf-convert
    :doc "start up 画面を表示しない"
    :setq ((inhibit-startup-message . t)))

  (leaf toolbar
    :doc "ツールバーとスクロールバーを消す"
    :custom ((if)
	     (display-graphic-p)
	     (progn)

	     (tool-bar-mode)
	     (scroll-bar-mode)))

  (leaf menubar
    :doc "メニューバーを消す"
    :custom ((menu-bar-mode)))

  (leaf vbell
    :setq ((visible-bell . t)
	   (ring-bell-function quote ignore)))

  (leaf backup-files
    :doc "ロックファイル / バックアップファイルを作成しない"
    (leaf backup-files
      :setq ((create-lockfiles)
	     (make-backup-files)
	     (delete-auto-save-files)
	     (auto-save-default))))

  (leaf leaf-convert
    :doc "バックアップファイルはカレントディレクトリではなく~/.emacs.d/backups 以下に保存する"
    :config
    (setq backup-directory-alist `(("." \,
				    (concat user-emacs-directory "backups")))))

  (leaf leaf-convert
    :doc "vbell off"
    :setq ((visible-bell . t)
	   (ring-bell-function quote ignore)))

  (leaf leaf-convert
    :doc "ミニバッファの履歴を保存"
    :setq ((savehist-mode . 1)))

  (leaf leaf-convert
    :setq-default ((tab-width . 4)
		 (indent-tabs-mode)))

  (leaf leaf-convert
    :doc "現在行に色をつける"
    :config
    (global-hl-line-mode 1))

  (leaf leaf-convert
    :doc " 同じ内容を履歴に記録しないようにする"
    :setq ((history-delete-duplicates . t)))

  (leaf leaf-convert
    :config
    (setenv "LANG" "en_US.UTF-8"))

  (leaf leaf-convert
    :doc "右から左に読む言語に対応させないことで描画高速化"
    :setq-default ((bidi-display-reordering)))

  (leaf leaf-convert
    :doc "対応する括弧の表示"
    :config
    (show-paren-mode 1))

  (leaf leaf-convert
    :doc "ウィンドウの記録"
    :config
    (desktop-save-mode 1))

  (leaf recentf
    :doc "最近開いたファイル履歴を保存"
    :custom ((recentf-max-saved-items . 1000)
	   (recentf-auto-cleanup . t)
	   (recentf-exclude quote
			    (".recentf")))
    :config
    (recentf-mode 1))

  (leaf warnings
    :doc "起動時の警告を抑制"
    :setq ((inhibit-startup-message . t)))

  (leaf row-number
    :doc "行番号表示"
    :config
    (global-display-line-numbers-mode t)))


(leaf exec-path-from-shell
  :doc ローカル環境変数の読み込み
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )


(leaf leaf-convert
  :doc "macクリップボードの共有"
  :preface
  (defun copy-from-osx nil
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))

  :setq ((interprogram-cut-function quote paste-to-osx)
	 (interprogram-paste-function quote copy-from-osx)))


(leaf leaf-convert
  :doc ""switch meta between Option and Command""
  :init (setq mac-command-modifier 'meta)
  :preface
  (defun mac-switch-meta nil
    "switch meta between Option and Command"
    (interactive)
    (if (eq mac-option-modifier nil)
        (progn
          (setq mac-option-modifier 'meta)
          (setq mac-command-modifier 'hyper))

      (progn
        (setq mac-option-modifier nil)
        (setq mac-command-modifier 'meta)))))


(leaf switch-buffer
  :doc "バッファの行き来"
  :bind (("M-[" . switch-to-prev-buffer)
	 ("M-]" . switch-to-next-buffer)))

(leaf edit-scratch
  :init
  (defvar toggle-scratch-prev-buffer nil)
  :preface
  (defun toggle-scratch nil
    "go to *scratch* buffer"
    (interactive)
    (if (not (string= "*scratch*"
                      (buffer-name)))
        (progn
          (setq toggle-scratch-prev-buffer (buffer-name))
          (switch-to-buffer "*scratch*"))
      (switch-to-buffer toggle-scratch-prev-buffer)))
  :bind (("C-]" . toggle-scratch))
  :setq ((initial-major-mode quote text-mode)))


(leaf leaf-convert
  :preface
  (defun narrow-or-widen-dwim nil
    "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
    (interactive)
    (cond
     ((buffer-narrowed-p)
      (widen)
      (recenter))
     ((region-active-p)
      (narrow-to-region
       (region-beginning)
       (region-end)))
     (t
      (error "Please select a region to narrow to"))))
  :bind (("C-." . narrow-or-widen-dwim)))


(leaf undo-tree
  :doc "undo redo"
  :ensure t
  :config
  (global-undo-tree-mode t)
  :custom ((undo-tree-auto-save-history . nil))
  )

(leaf smartparens
  :doc "カッコの処理"
  :ensure t
  ;; :hook (after-init-hook . smartparens-global-strict-mode) ; strictモードを有効化
  :require smartparens-config
  :custom ((electric-pair-mode . nil))) ; electirc-pair-modeを無効化


(leaf tab-bar
  :doc "タブバーモードを有効にする"
  :bind (("C-TAB" . tab-bar-move-tab)
         ("C-S-TAB" . tab-bar-switch-to-next-tab))
  :config
  (tab-bar-mode 1))

(leaf leaf-convert
  :doc "diredで直接ファイル名編集"
  :require wdired
  :config
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))


(leaf ripgrep
  :doc "高速grep"
  :ensure t
  :custom ((ripgrep-arguments . '("-S")))
)

(leaf yaml-mode
  :doc "yaml"
  :ensure t
)

(leaf color-theme
  :config
  (leaf idea-darkula-theme
    :ensure t
    :config (load-theme 'idea-darkula t))
  (leaf solarized-theme
    :ensure t
    :disabled t
    :config (load-theme 'solarized-dark t))
 )

(leaf visual-regexp-steroids
  :doc "視覚的正規表現"
  :custom ((vr/engine quote python)))


(leaf easy-kill
  :doc "キリングしないでコピー"
  :ensure t
  :bind (("M-w" . easy-kill)))

(leaf vertico-setting
  :doc "検索インターフェース関連"
  :config
  (leaf consult
    :doc "Consulting completing-read"
    :req "emacs-27.1" "compat-28.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/consult"
    :added "2022-05-22"
    :emacs>= 27.1
    :ensure t
    :bind (("C-c h" . consult-history)
    ("C-c m" . consult-mode-command)
    ("C-c b" . consult-bookmark)
    ("C-c k" . consult-kmacro)
    ("C-x M-:" . consult-complex-command)
    ("C-x b" . consult-buffer)
    ("C-x 4 b" . consult-buffer-other-window)
    ("C-x 5 b" . consult-buffer-other-frame)
    ("M-#" . consult-register-load)
    ("M-'" . consult-register-store)
    ("C-M-#" . consult-register)
    ("M-y" . consult-yank-pop)
    ;; (" a" . consult-apropos)
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake)
    ("M-g g" . consult-goto-line)
    ("M-g M-g" . consult-goto-line)
    ("M-g o" . consult-outline)
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ("M-s f" . consult-find)
    ("M-s F" . consult-locate)
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s r" . consult-ripgrep)
    ("M-s l" . consult-line)
    ("M-s L" . consult-line-multi)
    ("M-s m" . consult-multi-occur)
    ("M-s k" . consult-keep-lines)
    ("M-s u" . consult-focus-lines)
    ("M-s e" . consult-isearch)
    (isearch-mode-map
     ("M-e" . consult-isearch)
     ("M-s e" . consult-isearch)
     ("M-s l" . consult-line)
     ("M-s L" . consult-line-multi)))
    )
  (leaf orderless
    :doc "Completion style for matching regexps in any order"
    :req "emacs-26.1"
    :tag "extensions" "emacs>=26.1"
    :url "https://github.com/oantolin/orderless"
    :added "2021-09-04"
    :emacs>= 26.1
    :setq ((completion-styles quote
         (orderless)))
    :ensure t)
  
  (leaf vertico
    :doc "VERTical Interactive COmpletion"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/vertico"
    :added "2021-09-04"
    :emacs>= 27.1
    :ensure t)
  
  (leaf marginalia
    :doc "Enrich existing commands with completion annotations"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/marginalia"
    :added "2022-05-22"
    :emacs>= 27.1
    :ensure t)

  (leaf init_hook
    :preface
    (defun after-init-hook nil
      (vertico-mode)
      (marginalia-mode)
      (savehist-mode))
    :hook ((after-init-hook . after-init-hook)))
  
  (leaf embark
    :doc "Conveniently act on minibuffer completions"
    :req "emacs-26.1"
    :tag "convenience" "emacs>=26.1"
    :url "https://github.com/oantolin/embark"
    :added "2021-09-04"
    :emacs>= 26.1
    :ensure t)
  
  (leaf embark-consult
    :doc "Consult integration for Embark"
    :req "emacs-25.1" "embark-0.9" "consult-0.1"
    :tag "convenience" "emacs>=25.1"
    :url "https://github.com/oantolin/embark"
    :added "2021-09-04"
    :emacs>= 25.1
    :ensure t
    :after embark consult)
  )


(leaf highlight-indent-guides
  :doc "インデント位置を強調"
  :ensure t
  :blackout t
  :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom (
           (highlight-indent-guides-method . 'character)
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?\|)))


(leaf rainbow-delimiters
  :doc "カッコの対応を強調"
  :ensure t
  :hook
  ((prog-mode-hook       . rainbow-delimiters-mode)))



(leaf company
  :doc "補完"
  :ensure t
  :leaf-defer nil
  :blackout company-mode
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-i" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-tooltip-limit         . 12)
           (company-idle-delay            . 0) ;; 補完の遅延なし
           (company-minimum-prefix-length . 1) ;; 1文字から補完開始
           (company-transformers          . '(company-sort-by-occurrence))
           (global-company-mode           . t)
           (company-selection-wrap-around . t)))


(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/justbur/emacs-which-key"
  :added "2022-05-22"
  :emacs>= 24.4
  :ensure t
  :config
  (which-key-mode);; (which-key-setup-minibuffer)
  )

(leaf persistent-scratch
  :doc "scrachの永続化"
  :ensure t
  :config
  (persistent-scratch-setup-default))


(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-display-errors-delay . 0.3)
           (flycheck-indication-mode . 'left-margin)) ;terminalで使うので、fringeではなくmarginに警告を表示
  :config (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode) ; flycheckのみでmarginを使用
    (leaf flycheck-inline
      :ensure t
      :hook (flycheck-mode-hook . flycheck-inline-mode)))


(leaf lsp-mode
  :doc "言語ごとの解析サーバーと連携"
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  :custom ((lsp-keymap-prefix                  . "C-c l")
           (lsp-log-io                         . t)
           (lsp-keep-workspace-alive           . nil)
           (lsp-document-sync-method           . 2)
           (lsp-response-timeout               . 5)
           (lsp-enable-file-watchers           . nil))
  :hook (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :init (leaf lsp-ui
          :ensure t
          :after lsp-mode
          :custom ((lsp-ui-doc-enable            . t)
                   (lsp-ui-doc-position          . 'at-point)
                   (lsp-ui-doc-header            . t)
                   (lsp-ui-doc-include-signature . t)
                   (lsp-ui-doc-max-width         . 150)
                   (lsp-ui-doc-max-height        . 30)
                   (lsp-ui-doc-use-childframe    . nil)
                   (lsp-ui-doc-use-webkit        . nil)
                   (lsp-ui-peek-enable           . t)
                   (lsp-ui-peek-peek-height      . 20)
                   (lsp-ui-peek-list-width       . 50))
          :bind ((lsp-ui-mode-map ([remap xref-find-definitions] .
                                   lsp-ui-peek-find-definitions)
                                  ([remap xref-find-references] .
                                   lsp-ui-peek-find-references))
                 (lsp-mode-map ("C-c s" . lsp-ui-sideline-mode)
                               ("C-c d" . lsp-ui-doc-mode)))
          :hook ((lsp-mode-hook . lsp-ui-mode))))



(leaf dashboard
  :ensure t
  :require t
  :config
  (dashboard-setup-startup-hook))


(leaf projectile
  :ensure t
  :custom ((rojectile-enable-caching . t))
  )


(leaf neotree
  :init
  (setq-default neo-keymap-style 'concise)
  :bind (("C-," . neotree-toggle))
  :custom ((neo-persist-show . t)
           (neo-create-file-auto-open . t)
           (projectile-switch-project-action . "neotree-projectile-action")
           (neo-smart-open . t))
  :config
  (with-eval-after-load 'neotree
    (add-hook 'neo-after-create-hook
              (lambda (&rest _)
                (display-line-numbers-mode -1)))))

  (leaf warnings
    :doc "起動時の警告を抑制"
    :setq ((inhibit-startup-message . t)))



(leaf multiple-cursors
  :doc "intelij並の矩形編集実現"
  :ensure t
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-c" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)))


(leaf cperl-mode
  :commands lsp-deferred
  :hook ((cperl-mode-hook . lsp-deferred))
  :config
  (with-eval-after-load 'cperl-mode
    (cperl-set-style "PerlStyle"))
  :preface
  (defun perltidy-region nil
    "Run perltidy on the current region."
    (interactive)
    (save-excursion
      (shell-command-on-region
       (point)
       (mark)
       "perltidy -q" nil t)))

  (defun perltidy-defun nil
    "Run perltidy on the current defun."
    (interactive)
    (save-excursion
      (mark-defun)
      (perltidy-region)))
  )

(provide 'init)
;;; init.el ends here
