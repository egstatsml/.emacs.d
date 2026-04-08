 ;;; setup-editing.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; will auto save buffers for me a bit nicer
(use-package super-save
  :ensure t
  :config
  ;; add a command trigger (useful for commands that don't involve a buffer switch)
  (with-eval-after-load 'ace-window
    (add-to-list 'super-save-triggers 'ace-window))
  (setq super-save-remote-files nil)
  ;; Enable deleting trailing white spaces before saving (except for the current line)
  (setq super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-mode +1))

(provide 'setup-editing)
