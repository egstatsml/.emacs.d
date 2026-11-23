;;; setup-ai.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;;

(use-package emureader
  :ensure (emureader
           :type git :host codeberg :repo "MonadicSheep/emacs-reader"
  	   :files (:defaults "render-core.so")
  	   :pre-build ("make" "all"))
  :config
  (setq emureader-directory
        (expand-file-name "elpaca/sources/emacs-reader" user-emacs-directory)))

(provide 'setup-pdf)
