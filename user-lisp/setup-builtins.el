
;; maintain list of recently accessed files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 300) ; default is 20
  (setq recentf-auto-cleanup 'mode))


;; remember location of file when reopening
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode)
  (setq save-place-file (expand-file-name "saveplace" ethan/cache-dir))
  (setq save-place-limit 600))


(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-file (expand-file-name "savehist" ethan/cache-dir))
  (setq savehist-save-minibuffer-history t
	savehist-autosave-interval nil     ; save on kill only
	savehist-additional-variables
	'(kill-ring                        ; persist clipboard
	  register-alist                   ; persist macros
	  mark-ring global-mark-ring       ; persist marks
	  search-ring regexp-search-ring))) ; persist searches


(provide 'setup-emacs)
