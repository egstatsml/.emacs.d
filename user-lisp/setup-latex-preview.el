;;; setup-latex-preview.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package latex-to-svg-backend
  :ensure (latex-to-svg-backend
           :host github
           :repo "alberti42/latex-to-svg-backend"))


(use-package latex-to-svg-frontend
  :ensure (latex-to-svg-frontend
           :host github
           :repo "alberti42/latex-to-svg"))

;; will use the builtin org latex preview now
;; but use this for agent shell
(use-package agent-shell-math-renderer
  :ensure (agent-shell-math-renderer
           :host github
           :repo "alberti42/agent-shell-math-renderer")
  :hook (agent-shell-mode . agent-shell-math-renderer-mode)
  ;; Optional: re-tint equations the instant you switch themes, and
  ;; rescale them when the frame font changes.  See "Usage" below; omit
  ;; if you never change themes or font sizes at runtime.
  :config
  (add-hook 'enable-theme-functions
            #'agent-shell-math-renderer-on-appearance-change)
  (add-hook 'after-setting-font-hook
            #'agent-shell-math-renderer-on-appearance-change))

(provide 'setup-latex-preview)
