;;; setup-git.el --- Git settings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ethan Goan
;;
;; Author: Ethan Goan <ethan@work>
;; Maintainer: Ethan Goan <ethan@work>
;; Created: November 16, 2026
;; Modified: November 16, 2026
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/Symbol’s function definition is void: doom-call-process/setup-git
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  some Git settings for me
;;
;;; Code:


(hel-keymap-set mode-specific-map
  "v RET" 'magit-status
  "v g"   'magit-status   ;; "C-x g"
  "v SPC" 'magit-dispatch
  "v /"   'magit-dispatch
  "v ?"   'magit-dispatch
  "v h"   'magit-dispatch ;; "h" in magit-status buffer
  "v f"   'magit-file-dispatch
  "f v"   'magit-file-dispatch)

(use-package magit
  :ensure t
  :hook ((magit-process-mode . goto-address-mode)
         (magit-popup-mode-hook hide-mode-line-mode))
  :config
  (magit-auto-revert-mode)
  :custom
  (magit-refresh-verbose debug-on-error)
  (magit-diff-refine-hunk t) ;; show granular diffs in selected hunk
  ;; magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))


(use-package remoto
  :ensure t)


(provide 'setup-git)
;;; setup-git.el ends here
