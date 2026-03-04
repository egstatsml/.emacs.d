;;; helheim-flycheck-lsp.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; lsp
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
	 (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
		       (fboundp 'json-parse-buffer))
		'json-parse-buffer
	      'json-read)
	    :around
	    #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
	     (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
	     lsp-use-plists
	     (not (functionp 'json-rpc-connection))  ;; native json-rpc
	     (executable-find "emacs-lsp-booster"))
	(progn
	  (message "Using emacs-lsp-booster for %s!" orig-result)
	  (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-mode
  :ensure (lsp-mode
	   :host github
	   :repo "emacs-lsp/lsp-mode")
  :commands lsp-install-server
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :config
  ;; disable lsp signature
  (setq lsp-signature-auto-activate nil)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :none) ;; we use Corfu!
  ;; config fn to be run as hook taken from corfu wiki
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf
					completion-category-defaults))
	  '(orderless))) ;; Configure orderless
  ;; stealing some lsp config for performance improvements from doom
  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; increase read process max
  ;; as suggested in LSP performance docs
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; chaining checkers
  ;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442
  (defvar-local my/flycheck-local-cache nil)
  (defun my/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker my/flycheck-local-cache))
	(funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
  ;; example of how to use it
  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'sh-mode)
  ;;		(setq my/flycheck-local-cache '((lsp . ((next-checkers . (sh-posix-bash))))))))))
  ) ;; end use-package lsp


(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :bind ("C-c C-d" . 'lsp-ui-doc-show)
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
        ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  (define-key lsp-ui-mode-map
	      [remap xref-find-definitions]
	      #'lsp-ui-peek-find-definitions)

  (define-key lsp-ui-mode-map
	      [remap xref-find-references]
	      #'lsp-ui-peek-find-references))


(use-package consult-lsp
  :ensure t
  :after lsp-mode
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred


;;; flycheck
(use-package flycheck
  :ensure
  ;;:hook (elpaca-after-init . global-flycheck-mode)
  :config
  (add-hook 'elpaca-after-init-hook #'global-flycheck-mode)
  ;; fixing wierd bug with rustc
  (flycheck-define-checker rust
    "A Rust syntax checker using Rust compiler.

This syntax checker needs Rust 1.18 or newer.  See URL
`https://www.rust-lang.org'."
    :command ("rustc"
              (option "--crate-type" flycheck-rust-crate-type)
              "--emit=metadata" "--out-dir" (eval (flycheck-temp-dir-system)) ; avoid creating binaries
              "--error-format=json"
              (option-flag "--test" flycheck-rust-check-tests)
              (option-list "-L" flycheck-rust-library-path concat)
              (eval flycheck-rust-args)
              (eval (or flycheck-rust-crate-root
			(flycheck-substitute-argument 'source-original 'rust))))
    :error-parser flycheck-parse-rustc
    :error-filter flycheck-rust-error-filter
    :error-explainer flycheck-rust-error-explainer
    :modes (rust-mode rust-ts-mode)
    :predicate flycheck-buffer-saved-p)

  ;; stealing some config from doom
  ;; Rerunning checks on every newline is a mote excessive
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)
  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t))


(use-package flyover
  :ensure t
  :hook ((flycheck-mode . flyover-mode)
         (flymake-mode . flyover-mode))
  :custom
  ;; Checker settings
  (flyover-checkers '(flycheck flymake))
  (flyover-levels '(error warning info))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)
  (flyover-text-tint 'lighter)
  (flyover-text-tint-percent 50)

  ;; Icons
  (flyover-info-icon " ")
  (flyover-warning-icon " ")
  (flyover-error-icon " ")

  ;; Display settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-line-position-offset 1)
  ;; show at end of the line instead.
  (flyover-show-at-eol t)
  ;; Hide overlay when cursor is at same line, good for show-at-eol.
  (flyover-hide-when-cursor-is-on-same-line t)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2))

;; consult-flycheck
(use-package consult-flycheck
  :ensure t
  :after (consult flycheck))

(provide 'helheim-flycheck-lsp)
