;;; setup-projectile.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; rg
;; used by projectile
;;
(use-package rg
  :ensure t
  :defer t)
;;; projectile
;;
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/")
	projectile-cache-file (concat ethan/cache-dir "projectile.cache")
	projectile-enable-caching (not noninteractive)
	projectile-globally-ignored-files '(".DS_Store" "TAGS")
	projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
	projectile-kill-buffers-filter 'kill-all
	projectile-known-projects-file (concat ethan/cache-dir "projectile.projects")
	;; auto discover slow for tramp, can use M-x projectile-discover-projects-in-directory
	projectile-auto-discover nil)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
	compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  ;; making sure that ripgrep/rg will search through hidden files
  ;; this is really handy when working with my init/config files, of which most
  ;; are under a hidden directory
  (setq rg-command-line-flags '("--hidden")))
(provide 'setup-projectile)
