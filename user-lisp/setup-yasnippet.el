;;; setup-yasnippet.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package yasnippet
  :ensure t
  :commands (yas-expand
	     yas-expand-snippet
	     yas-lookup-snippet
	     yas-insert-snippet
	     yas-new-snippet
	     yas-visit-snippet-file
	     yas-activate-extra-mode
	     yas-deactivate-extra-mode
	     yas-maybe-expand-abbrev-key-filter)
  ;; :bind (("C-c i s" . yas-insert-snippet))
  :hook ((prog-mode . my/enable-yas)
	 (org-mode . my/enable-yas)
	 (LaTeX-mode . my/enable-yas))
  :init
  (defun my/enable-yas ()
    "Helper function to enable yas"
    (yas-global-mode 1))
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (defvar yas-verbosity 2)
  ;; Remove default ~/.emacs.d/snippets
  (setq yas-snippet-dirs (list (concat helhei-directory "snippets")))
  ;; get functionality from yasnippet-snippets repo that I have copied across
  ;; (load-file (concat user-emacs-directory
  ;;		     "snippets/yasnippet-snippets.el"))
  :config
  ;; making sure that yas is compatible with cdlatex
  ;; cdlatex uses the tab key very heavily, but I also want tab to work nicely
  ;; with the yas. Karthink has sorted this out so am going to yoink this and
  ;; see how I go
  (with-eval-after-load 'cdlatex
    ;; Allow cdlatex tab to work inside Yas fields
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
		  (end (overlay-end yas--active-field-overlay)))
	(if (>= (point) end)
	    ;; Call yas-next-field if cdlatex can't expand here
	    (let ((s (thing-at-point 'sexp)))
	      (unless (and s (assoc (substring-no-properties s)
				    cdlatex-command-alist-comb))
		(yas-next-field-or-maybe-expand)
		t))
	  ;; otherwise expand and jump to the correct location
	  (let (cdlatex-tab-hook minp)
	    (setq minp
		  (min (save-excursion (cdlatex-tab)
				       (point))
		       (overlay-end yas--active-field-overlay)))
	    (goto-char minp) t))))

    (add-hook 'cdlatex-tab-hook #'yas-expand)
    ;; (add-hook 'cdlatex-tab-hook #'cdlatex-in-yas-field)
    ;; (define-key yas-keymap (kbd "TAB")
    (define-key yas-keymap [tab] 'yas-next-field-or-cdlatex)
    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
	  (or (bound-and-true-p cdlatex-mode)
	      (bound-and-true-p org-cdlatex-mode))
	  (cdlatex-tab)
	(yas-next-field-or-maybe-expand))))
  (yas-global-mode +1))

(use-package yasnippet-capf
  :ensure (yasnippet-capf
	   :host github
	   :repo "elken/yasnippet-capf")
  :after (yasnippet corfu)
  :config
  (add-hook 'yas-minor-mode-hook
	    (lambda () (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

(provide 'setup-yasnippet)
