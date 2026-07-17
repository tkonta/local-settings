;;; init.el --- my init.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs settings.  Packages are configured with use-package.
;;; Code:

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory
            (or load-file-name byte-compile-current-file))))))

;; Emacs Lisp packages are installed by Nix/Home Manager.
;; use-package is responsible only for configuration.
(require 'use-package)

(setq use-package-always-ensure nil
      use-package-always-defer nil)

;;;; Basic environment

(setq dired-use-ls-dired nil
      inhibit-startup-message t
      visible-bell t
      ring-bell-function #'ignore
      create-lockfiles nil
      make-backup-files nil
      delete-auto-save-files nil
      auto-save-default nil
      history-delete-duplicates t
      backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

(setq-default tab-width 4
              indent-tabs-mode nil
              bidi-display-reordering nil)

;; 日本語環境でのバックスラッシュ入力
(define-key global-map [?¥] [?\\])

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(desktop-save-mode 1)
(savehist-mode 1)
(global-display-line-numbers-mode 1)
(setenv "LANG" "en_US.UTF-8")

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup t)
  (recentf-exclude '(".recentf"))
  :config
  (recentf-mode 1))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;;; macOS integration

(defun copy-from-osx ()
  "Return the current macOS clipboard contents."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional _push)
  "Copy TEXT to the macOS clipboard."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function #'paste-to-osx
      interprogram-paste-function #'copy-from-osx
      mac-command-modifier 'meta)

(defun mac-switch-meta ()
  "Switch Meta between Option and Command."
  (interactive)
  (if (eq mac-option-modifier nil)
      (setq mac-option-modifier 'meta
            mac-command-modifier 'hyper)
    (setq mac-option-modifier nil
          mac-command-modifier 'meta)))

;;;; Editing and navigation

(global-set-key (kbd "M-[") #'switch-to-prev-buffer)
(global-set-key (kbd "M-]") #'switch-to-next-buffer)

(defvar toggle-scratch-prev-buffer nil
  "Buffer shown immediately before switching to *scratch*.")

(defun toggle-scratch ()
  "Toggle between the current buffer and *scratch*."
  (interactive)
  (if (not (string= "*scratch*" (buffer-name)))
      (progn
        (setq toggle-scratch-prev-buffer (buffer-name))
        (switch-to-buffer "*scratch*"))
    (switch-to-buffer toggle-scratch-prev-buffer)))

(global-set-key (kbd "C-]") #'toggle-scratch)
(setq initial-major-mode 'text-mode)

(defun narrow-or-widen-dwim ()
  "Widen a narrowed buffer, or narrow it to the active region."
  (interactive)
  (cond
   ((buffer-narrowed-p)
    (widen)
    (recenter))
   ((region-active-p)
    (narrow-to-region (region-beginning) (region-end)))
   (t
    (user-error "Please select a region to narrow to"))))

(global-set-key (kbd "C-.") #'narrow-or-widen-dwim)

(use-package macrostep
  :bind ("C-c e" . macrostep-expand))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (electric-pair-mode -1))

(use-package tab-bar
  :bind (("C-TAB" . tab-bar-move-tab)
         ("C-S-TAB" . tab-bar-switch-to-next-tab))
  :config
  (tab-bar-mode 1))

(with-eval-after-load 'dired
  (require 'wdired)
  (define-key dired-mode-map (kbd "r") #'wdired-change-to-wdired-mode))

(use-package ripgrep
  :custom
  (ripgrep-arguments '("-S")))

(use-package yaml-mode
)

;;;; Appearance

(use-package idea-darkula-theme
  :config
  (load-theme 'idea-darkula t))

(use-package solarized-theme
  :disabled t
  :config
  (load-theme 'solarized-dark t))

(use-package visual-regexp-steroids
  :custom
  (vr/engine 'python))

(use-package easy-kill
  :bind ("M-w" . easy-kill))

;;;; Minibuffer completion

(use-package consult
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
         :map isearch-mode-map
         ("M-e" . consult-isearch)
         ("M-s e" . consult-isearch)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package embark
)

(use-package embark-consult
  :after (embark consult))

;;;; Programming support

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-character ?\|)
  :config
  (setq minor-mode-alist
        (assq-delete-all 'highlight-indent-guides-mode minor-mode-alist)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :demand t
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-s" . company-filter-candidates)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-i" . company-complete-selection)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-tooltip-limit 12)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-transformers '(company-sort-by-occurrence))
  (company-selection-wrap-around t)
  :config
  (global-company-mode 1)
  (setq minor-mode-alist
        (assq-delete-all 'company-mode minor-mode-alist)))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.3)
  (flycheck-indication-mode 'left-margin)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode))

(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-headerline-breadcrumb-mode)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-log-io t)
  (lsp-keep-workspace-alive nil)
  (lsp-document-sync-method 2)
  (lsp-response-timeout 5)
  (lsp-enable-file-watchers nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              :map lsp-mode-map
              ("C-c s" . lsp-ui-sideline-mode)
              ("C-c d" . lsp-ui-doc-mode))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50))

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook))

(use-package projectile
  :custom
  (projectile-enable-caching t))

(use-package neotree
  :init
  (setq-default neo-keymap-style 'concise)
  :bind ("C-," . neotree-toggle)
  :custom
  (neo-persist-show t)
  (neo-create-file-auto-open t)
  (projectile-switch-project-action #'neotree-projectile-action)
  (neo-smart-open t)
  :config
  (add-hook 'neo-after-create-hook
            (lambda (&rest _)
              (display-line-numbers-mode -1))))

(use-package multiple-cursors
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-c" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)))

(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point) (mark) "perltidy -q" nil t)))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (perltidy-region)))

(use-package cperl-mode
  :hook (cperl-mode . lsp-deferred)
  :config
  (cperl-set-style "PerlStyle"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
