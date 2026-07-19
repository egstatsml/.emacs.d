;;; setup-vundo.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package undo-fu
  :ensure t
  :init
  ;; increasing undo limits
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb.
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))


(use-package vundo
  :ensure (vundo
	   :host github
	   :repo "casouri/vundo")
  :bind
  ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
	vundo-compact-display t))
  ;; this is to ensure that the tree is all aligned in vundo
  ;; just explicitly forcing it to use a monospace font here.
  ;; (set-face-attribute 'vundo-default nil :font "JetBrainsMono Nerd Font Mono") :family "Iosevka Nerd Font"))
(provide 'setup-vundo)
