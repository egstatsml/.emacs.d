;;; setup-debug.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package dap-mode
  :ensure t
  :config
  ;; auto configure
  (dap-auto-configure-mode)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  ;; languages
  ;; python
  (require 'dap-python)
  ;; if you installed debugpy, you need to set this
  ;; https://github.com/emacs-lsp/dap-mode/issues/306
  (setq dap-python-debugger 'debugpy))

(provide 'setup-debug)
