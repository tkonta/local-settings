;; load-path
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;el-get install
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


;; env
(el-get-bundle magit)
(el-get-bundle ag)
(el-get-bundle auto-complete)
(el-get-bundle browse-kill-ring)
(el-get-bundle elscreen)
(el-get-bundle elscreen-persist)
(el-get-bundle flycheck)
(el-get-bundle git-gutter)
(el-get-bundle quickrun)
(el-get-bundle zlc)
(el-get-bundle smartparens)
(el-get-bundle direx)
(el-get-bundle projectile)
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-swoop)
(el-get-bundle helm-projectile)
(el-get-bundle exec-path-from-shell);
(el-get-bundle undo-tree)
(el-get-bundle migemo)
(el-get-bundle point-undo)
(el-get-bundle yasnippet)
(el-get-bundle popwin)


;; progs mode
(el-get-bundle coffee-mode)
(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle go-eldoc)
(el-get-bundle go-direx)
(el-get-bundle js2-mode)
(el-get-bundle json-mode)
(el-get-bundle markdown-mode)
(el-get-bundle yaml-mode)
(el-get-bundle web-mode)
(el-get-bundle org-mode)
(el-get-bundle php-mode)
(el-get-bundle flymake-go)
(el-get-bundle atotto/yasnippet-golang)


;;---
;; start up 画面を表示しない
(setq inhibit-startup-message t)


;;vbell off
(setq visible-bell t) 
(setq ring-bell-function 'ignore)


;; wdiree: dired で直接ファイルをリネーム
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; undo-tree
; C-u でビジュアルモード
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)


;;---
;; すべてのモードでgit差分を有効に
(global-git-gutter-mode t)

;;--
;; バッファ内でコード実行
(global-set-key (kbd "<f5>") 'quickrun)

;; tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; clip board
(setq x-select-enable-clipboard t)

;; color-theme
;(load-theme 'misterioso t)
(load-theme 'wombat t)

;; 行番号表示
;(global-linum-mode t)

;;行末の空白をハイライト
(setq-default show-trailing-whitespace t)

;;()対応
(require 'smartparens-config)
(smartparens-global-mode t)

;; 対応する括弧を表示してくれる
(show-paren-mode 1)

;; auto-complete
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)

;; バッファの行き来
(global-set-key (kbd "M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "M-]") 'switch-to-next-buffer)

;;カーソル位置履歴
(require 'point-undo)
(global-set-key (kbd "C-M-[") 'point-undo)
(global-set-key (kbd "C-M-]") 'point-redo)

;; ファイルを開いた時に前回のカーソル位置を復元
;; (require 'saveplace)
;; (setq-default save-place t)


;; killのバッファーに入れないバージョン
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "M-h") 'backward-delete-word)

;; 現在行に色を付ける
;(global-hl-line-mode t)

;;ツールバーとスクロールバーを消す
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; バックアップファイルはカレントディレクトリではなく
;; ~/.emacs.d/backups 以下に保存する
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; 保存時にファイル末尾に改行を入れる
require-final-newline t

;;popwin
(require 'popwin)
(popwin-mode 1)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)

;;---
;; shellの環境変数を共有

(let ((envs '("PATH" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;スニペット
(yas-global-mode 1)
;;; スニペット名をidoで選択する
(setq yas-prompt-functions '(yas-ido-prompt))


;; elscreen
;; プレフィックスキー
(setq elscreen-prefix-key (kbd "C-t"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))



;; elscreenのsession復元
(elscreen-persist-mode 1)

;;---
;;direx
;;(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;;---
;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;;switch project
;; C-c p p
;;switch project
;; C-c p f
;;current project file
;; C-c p F
;;all project file



;;---
;;helm
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates (lambda ()
                  (let ((cmds))
                    (mapatoms
                     (lambda (elt) (when (commandp elt) (push elt cmds))))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "A simple helm source for Emacs commands.")

(defvar helm-source-emacs-commands-history
  (helm-build-sync-source "Emacs commands history"
    :candidates (lambda ()
                  (let ((cmds))
                    (dolist (elem extended-command-history)
                      (push (intern elem) cmds))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "Emacs commands history")

(custom-set-variables
 '(helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-files-in-current-dir
                               helm-source-emacs-commands-history
                               helm-source-emacs-commands
                               )))

(define-key global-map (kbd "C-;") 'helm-mini)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)


(require 'helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogrou")
(global-set-key (kbd "C-c s") 'helm-ag)


;; helmキャッシュクリア
;; (defadvice file-cache-clear-cache (after helm-advice-file-cache-clear-cache activate)
;;   (setq helm-file-cache-initialized-p nil)
;;   (setq helm-file-cache-files nil))


;;------------------------------------------
;; migemo 日本語検索
;; brew install cmigemo --HEAD

(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;;---
;; syntax check flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; M-p/M-n で警告/エラー行の移動
(global-set-key "\M-p" 'flycheck-next-error)
(global-set-key "\M-n" 'flycheck-previous-error)
(define-key global-map (kbd "\C-cd") 'flycheck-list-errors)



;;---
;; php-mode

(setq php-mode-coding-style (quote psr2))
(when (require 'php-mode nil t)
  (add-hook 'php-mode-hook
            (lambda ()
              (setq flycheck-phpcs-standard "PSR2")
              (hs-minor-mode 1)
              (setq c-basic-offset 4)
              (c-set-offset 'case-label' 4)
              (c-set-offset 'arglist-intro' 4)
              (c-set-offset 'arglist-cont-nonempty' 4)
              (c-set-offset 'arglist-close' 0)))

  (setq auto-mode-alist
        (append '(("\\.php$"  . php-mode)
                  ("\\.ctp$" . php-mode))
                auto-mode-alist))

  )


;;sudo php /usr/lib/php/install-pear-nozlib.phar
;;sudo pear install PHP_CodeSniffer
;;sudo php /usr/lib/php/install-pear-nozlib.phar
;;sudo cp /etc/php.ini.default /etc/php.ini
;;add php.ini ... include_path = ".:/php/includes:/usr/lib/php/pear"





;;---
;;golang

;; go get github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/golang/lint/golint
;; go get github.com/kisielk/errcheck
;; go get -u github.com/jstemmer/gotags
;; go get golang.org/x/tools/cmd/goimports


;;補完
;(add-hook 'go-mode-hook 'flycheck-mode)

(setq gofmt-command "goimports")   ; 追加
(add-hook 'before-save-hook 'gofmt-before-save)


(add-hook 'go-mode-hook (lambda()
                          (require 'flymake-go)
                          (require 'go-autocomplete)
                          (setq display-buffer-function 'popwin:display-buffer)
                          (push '("^\*go-direx:" :regexp t :position left :width 0.3 :dedicated t)
                                popwin:special-display-config)
                          (define-key go-mode-map (kbd "C-x j") 'go-direx-pop-to-buffer)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          ;(local-set-key (kbd "M-.") 'godef-jump-other-window)
                          (setq indent-tabs-mode nil)    ; タブを利用
                          (setq c-basic-offset 4)        ; tabサイズを4にする
                          (setq tab-width 4)
                          ))

