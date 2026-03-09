;;; setup-ai.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;;
(use-package acp
  :ensure (acp
	   :host github
	   :repo "xenodium/acp.el"))

(use-package shell-maker
  :ensure t)

(use-package agent-shell
  :ensure (agent-shell
	   :host github
	   :repo "xenodium/agent-shell"))


(use-package agent-review
  :ensure (agent-review
	   :host github
	   :repo "nineluj/agent-review"))

(provide 'setup-ai)
