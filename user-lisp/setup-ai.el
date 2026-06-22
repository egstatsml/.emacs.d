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
	   :repo "xenodium/agent-shell"))


(use-package agent-review
  :ensure (agent-review
	   :host github
	   :repo "nineluj/agent-review"))


;; experimental agent shell with tramp
(use-package agent-shell-tramp-rpc
  :ensure (agent-shell-tramp-rpc
             :host github
             :repo "csheaff/agent-shell-tramp-rpc")
  :after agent-shell
  :config
  (agent-shell-tramp-rpc-mode 1))

(use-package gptel
  :ensure t)

(provide 'setup-ai)
