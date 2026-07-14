;;; setup-ai.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;;

(use-package reader
  :ensure (reader
           :type git :host codeberg :repo "MonadicSheep/emacs-reader"
  	   :files (:defaults "render-core.so")
  	   :pre-build ("make" "all")))

(provide 'setup-pdf)
