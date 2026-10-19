;;; setup-latex-preview.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package latex-to-svg-backend
  :ensure (latex-to-svg-backend
           :host github
           :repo "alberti42/latex-to-svg-backend"))


(use-package latex-to-svg-frontend
  :ensure (latex-to-svg-frontend
           :host github
           :repo "alberti42/latex-to-svg"))

(use-package latex-to-svg-for-org
  :ensure (latex-to-svg-for-org
           :host github
           :repo "alberti42/latex-to-svg"
           :files ("latex-to-svg-for-org.el"))
  :hook (org-mode . latex-to-svg-for-org-mode)
  :init
  ;; Ignore `#+startup: latexpreview': it would run Org's own preview
  ;; before this mode turns on (see Troubleshooting in
  ;; docs/latex-to-svg-for-org.md).
  (with-eval-after-load 'org
    (setq org-startup-options
          (assoc-delete-all "latexpreview" org-startup-options))))

(provide 'setup-latex-preview)
