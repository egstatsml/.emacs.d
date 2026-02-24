;;; helheim-python.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package python
  :ensure nil
  ;; unbind C-c C-r, I use this for org-roam
  :bind (:map python-ts-mode-map
	      ("C-c C-r" . nil)))

;; tomlparse needed for pet
(use-package tomlparse
  :ensure t
  :init
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))


(use-package pet
  :ensure t
  :config
  (setq pet-debug t)
  ;; only using the fastest methods for finding pet stuffs
  (setq pet-find-file-functions '(pet-find-file-from-project-root
                                  pet-locate-dominating-file))
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;;; numpydoc
(use-package numpydoc
  ;; use main repo and not emacsmirror as is a bit behind
  ;; as of 07/10/23
  :ensure (numpydoc
	   :host github
	   :repo "douglasdavis/numpydoc.el")
  ;; this is needed as is loaded before python-mode-map
  :commands (numpydoc-generate)
  ;; :init
  ;; (define-key python-mode-map (kbd "C-c C-e C-d") 'numpydoc-generate)
  ;; (define-key python-ts-mode-map (kbd "C-c C-c C-d") 'numpydoc-generate)
  :config
  (setq numpydoc-insertion-style 'yas ;; use yasnippet style prompt
	numpydoc-insert-examples-block nil ;; no examples block
	numpydoc-insert-return-without-typehint nil ;; as it says
	numpydoc-auto-fill-paragraphs t)) ;; autofill my docs

(provide 'helheim-python)
