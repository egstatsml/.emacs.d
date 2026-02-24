;;; setup-vundo.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package vundo
  :ensure (vundo
	   :host github
	   :repo "casouri/vundo")
  :bind
  ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
	vundo-compact-display t)
  ;; this is to ensure that the tree is all aligned in vundo
  ;; just explicitly forcing it to use a monospace font here.
  (set-face-attribute 'vundo-default nil :font "Iosevka Nerd Font Mono" :family "Iosevka Nerd Font"))
(provide 'setup-vundo)
