;;; early-init.el --- Early Emacs startup settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Keep package.el from activating packages from the user package directory.
;; Emacs packages are supplied by Nix/Home Manager instead.
;;; Code:

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
