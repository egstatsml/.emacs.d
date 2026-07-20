;;; setup-ai.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;;
(use-package acp
  :ensure (acp
	   :host github
	   :repo "xenodium/acp.el"))

(use-package shell-maker
  :ensure (shell-maker
	   :host github
	   :repo "xenodium/shell-maker"))

(use-package agent-shell
  :ensure (agent-shell
	   :host github
	   :repo "xenodium/agent-shell")
  :config
  (setq agent-shell-thought-process-icon ""))


(use-package agent-review
  :ensure (agent-review
	   :host github
	   :repo "nineluj/agent-review"))

(use-package agent-shell-tramp
  :straight (:host github :repo "junyi-hou/agent-shell-tramp")
  :after agent-shell
  :config
  (agent-shell-tramp-mode 1))

(use-package hel-agent-shell
  :ensure (hel-agent-shell
           :host github
           :repo "anuvyklack/hel-agent-shell"
           :branch "main")
  :after agent-shell)

(use-package gptel
  :ensure t)

(provide 'setup-ai)
