;;; setup-latex.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun czm-tex-setup-environments-and-outline-regexp ()
  "Helper function setup LaTeX.
From CZM."
  (LaTeX-add-environments
   '("lemma" LaTeX-env-label)
   '("exercise" LaTeX-env-label)
   '("example" LaTeX-env-label)
   '("proposition" LaTeX-env-label)
   '("corollary" LaTeX-env-label)
   '("remark" LaTeX-env-label)
   '("definition" LaTeX-env-label)
   '("theorem" LaTeX-env-label))
  (setq-local outline-regexp
	      (concat "\\\\"
		      (regexp-opt (append latex-metasection-list
					  (mapcar #'car latex-section-alist)
					  '("bibliography"
					    "begin{thebibliography"))
				  t))))

(defun czm-widen-first (orig-fun &rest args)
  "Widen first and apply ORIG-FUN with ARGS.
Used from CZM to fix bug in TeX view."
  (save-restriction
    (widen)
    (apply orig-fun args)))


(defun czm-TeX-next-error-wrapper (&optional arg)
  "Next message wrapper for LaTeX.
ARG used for 'next-error' function."
  (interactive "P")
  (if
      (or (null (TeX-active-buffer))
	  (eq 'compilation-mode (with-current-buffer TeX-command-buffer
				  major-mode)))
      (TeX-next-error arg reparse)
    (next-error arg)))

(defun czm-TeX-previous-error-wrapper (&optional arg)
  "Previous message wrapper for LaTeX.
ARG used for 'preivous-error' function."
  (interactive "P")
  (if
      (or (null (TeX-active-buffer))
	  (eq 'compilation-mode (with-current-buffer TeX-command-buffer
				  major-mode)))
      (TeX-previous-error arg reparse)
    (previous-error arg)))

;; install auctex
(use-package latex
  :ensure (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
          :pre-build (("make" "elpa"))
          :build (:not elpaca--compile-info) ;; Make will take care of this step
          :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
          :version (lambda (_) (require 'auctex) AUCTeX-version))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map)
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . czm-tex-setup-environments-and-outline-regexp)
  ;; (LaTeX-mode . czm-tex-buffer-face)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . abbrev-mode)
  ;; (LaTeX-mode . toggle-word-wrap)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . (lambda ()
		  (setq fill-column 80)))
  :bind (:map LaTeX-mode-map
	      ("s-c" . preview-clearout-at-point)
	      ("s-q" . LaTeX-fill-buffer)
	      ("C-c C-n" . nil)
					; TeX-normal-mode
	      ("C-c #" . nil)
	      ;; ("M-n" . czm-TeX-next-error-wrapper)
	      ;; ("M-p" . czm-TeX-previous-error-wrapper)
	      ([remap next-error])
	      ([remap previous-error])
	      ("M-n" . next-error)
	      ("M-p" . previous-error)
	      ;;unbind Tex-command-region bound to C-c C-r
	      ;;I want to use for org roam
	      ("C-c C-r" . nil))
  :init
  (setq TeX-ignore-warnings "Package hyperref Warning: Token not allowed in a PDF string")
  (setq my-master-bib-file "~/.local/ref.bib")
  (setq font-latex-user-keyword-classes
	'(("macros"  (("includcegraphics" "[{["))  font-lock-function-name-face command)))

  ;; making sure tex files are auto interpreted as LaTeX mode
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  ;; some settings taken from doom
  ;; TODO - Review these settings
  (setq TeX-parse-self t ; parse on load
	TeX-auto-save t  ; parse on save
	;; Use hidden directories for AUCTeX files.
	TeX-auto-local ".auctex-auto"
	TeX-style-local ".auctex-style"
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	;; Don't start the Emacs server when correlating sources.
	TeX-source-correlate-start-server nil
	;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
	TeX-electric-sub-and-superscript t
	;; Just save, don't ask before each compilation.
	TeX-save-query nil)
  ;; make includegraphics by highlighted properly
  (setq font-latex-user-keyword-classes '(("mycommands" (("includegraphics" "[{[")) font-lock-function-name-face command)))
  ;; (setq font-latex-user-keyword-classes
  ;; '(("macros"  (("includcegraphics" "{"))  font-lock-function-name-face command)))
  ;; (put 'LaTeX-narrow-to-environment 'disabled nil)
  :config
  ;; set sioyek to be the main viewer
  (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
  (add-to-list 'TeX-file-extensions "tex\\.~[0-9a-f]+~")
  (advice-add 'TeX-view :around #'czm-widen-first) ; fixes bug in TeX-view
  ;; (require 'texmathp)
  (defun LaTeX-skip-verbatim (orig-fun &rest args)
    (if (or (eq major-mode 'latex-mode)
	    (eq major-mode 'LaTeX-mode))
	(let ((n 100))
	  (apply orig-fun args)
	  (while (and (LaTeX-verbatim-p) (> n 0))
	    (setq n (- n 1))
	    (apply orig-fun args)))
      (apply orig-fun args)))

  (dolist (f '(outline-next-heading
	       outline-previous-heading
	       outline-up-heading
	       outline-forward-same-level
	       outline-backward-same-level))
    (advice-add f :around #'LaTeX-skip-verbatim))
  :custom
  (preview-auto-cache-preamble t)
  (preview-image-type 'dvipng)
  (reftex-derive-label-parameters
   '(15 50 t 1 "-"
	("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
	t))
  :custom-face (preview-face ((t (:background unspecified)))))

;;  don't want foldout to include "bibliography"
(defun czm-LaTeX-outline-level-advice (orig-fun &rest args)
  (if (looking-at "\\\\bibliography\\|\\\\begin{thebibliography}") 1 (apply orig-fun args)))

(defun my-preview-tailor-factor-function ()
  "ez"
  (if (string-suffix-p ".lean" (buffer-file-name)) 0.6 0.833))

(use-package preview-tailor
  :ensure (preview-tailor
	   :host github
	   :repo "ultronozm/preview-tailor.el")
  :after latex
  :config
  (preview-tailor-init)
  :custom
  (preview-tailor-additional-factor-function #'my-preview-tailor-factor-function))

(use-package foldout
  :ensure nil
  :config
  (advice-add 'LaTeX-outline-level :around #'czm-LaTeX-outline-level-advice))


(use-package czm-tex-util
  :ensure (czm-tex-util
	   :host github
	   :repo "ultronozm/czm-tex-util.el")
  :after latex)

(defun czm-tex-quote-advice (&rest _)
  (when (and TeX-fold-mode
	     (looking-back "``\\(.*?\\)''"))
    (czm-tex-fold-quotes (match-beginning 0) (match-end 0))))

(defun czm-tex-fold-macro-previous-word ()
  (interactive)
  (if TeX-fold-mode
      (save-excursion
	(backward-word)
	(TeX-fold-item 'macro))))

(defun my-yank-after-advice (&rest _)
  "Fold any yanked ref or eqref."
  (when (and (or (eq major-mode 'latex-mode)
		 (eq major-mode 'LaTeX-mode))
	     TeX-fold-mode
	     (string-match "\\\\\\(ref\\|eqref\\){\\([^}]+\\)}"
			   (current-kill 0)))
    (czm-tex-fold-macro-previous-word)))

(defun czm-abbreviate-latex-mode-name ()
  (setq TeX-base-mode-name "L"))

(add-hook 'LaTeX-mode-hook #'czm-abbreviate-latex-mode-name)

(use-package czm-tex-fold
  :if nil
  :ensure (czm-tex-fold
	   :host github
	   :repo "ultronozm/czm-tex-fold.el")
  :after latex
  :bind
  (:map TeX-fold-mode-map
	("C-c C-o C-s" . czm-tex-fold-fold-section)
	("C-c C-o s" . czm-tex-fold-clearout-section))
  :demand
  :config
  (czm-tex-fold-set-defaults)
  (czm-tex-fold-install)
  :custom
  (czm-tex-fold-bib-file "~/doit/refs.bib")
  :hook
  (LaTeX-mode . tex-fold-mode))


(use-package reftex
  :after latex
  :defer 2
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :config
  (setq reftex-default-bibliography '(my-master-bib-file))
  (setq reftex-insert-label-flags '("sf" "sfte"))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-style-default-list '("Default" "AMSMath" "Cleveref"))
  (setq reftex-use-multiple-selection-buffers t))


(use-package consult-reftex
  :ensure (consult-reftex
	   :host github
	   :repo "karthink/consult-reftex")
  ;; :load-path "plugins/consult-reftex/"
  :after (reftex consult embark)
  :bind (:map reftex-mode-map
	      ("C-c C-n r"   . consult-reftex-insert-reference)
	      ("C-c M-." . consult-reftex-goto-label)
	      :map org-mode-map
	      ("C-c (" . consult-reftex-goto-label)
	      ("C-c )"   . consult-reftex-insert-reference))
  :config
  (setq consult-reftex-preview-function
	#'consult-reftex-make-window-preview
	consult-reftex-preferred-style-order
	'("\\eqref" "\\ref"))
  (consult-customize consult-reftex-insert-reference
		     :preview-key (list :debounce 0.3 'any)))


(defun czm-attrap-LaTeX-fixer-flymake (msg pos end)
  (cond
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")
		msg)
    (list (attrap-option 'fix-open-dquote
			 (delete-region pos (1+ pos))
			 (insert "``"))
	  (attrap-option 'fix-close-dquote
			 (delete-region pos (1+ pos))
			 (insert "''"))))
   ((s-matches? (rx "Non-breaking space (`~') should have been used.")
		msg)
    (attrap-one-option 'non-breaking-space
		       (if (looking-at (rx space))
			   (delete-region pos (1+ pos))
			 (delete-region (save-excursion (skip-chars-backward "\n\t ")
							(point))
					(point)))
		       (insert "~")))
   ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.")
		msg)
    (attrap-one-option 'use-interword-spacing
		       (delete-region (point)
				      (1+ (point)))
		       (insert "\\ ")))
   ((s-matches? (rx "Intersentence spacing (`\\@') should perhaps be used.")
		msg)
    (attrap-one-option 'use-intersentence-spacing
		       (insert "\\@")))
   ((s-matches? (rx "Delete this space to maintain correct pagereferences.")
		msg)
    ;; not yet fixed
    (attrap-one-option 'fix-space-pageref
		       (if (looking-back (rx bol (* space)))
			   (progn (skip-chars-backward "\n\t ")
				  (insert "%"))
			 (delete-region (point)
					(save-excursion (skip-chars-forward " \t")
							(point)))
			 )))
   ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.")
		msg)
    (attrap-one-option 'enclose-with-braces
		       (forward-char)
		       (insert "}")
		       (save-excursion
			 (backward-char)
			 (backward-sexp)
			 (re-search-backward "[^[:alnum:]\\_\\/]")
			 (forward-char)
			 (insert "{")
			 )))
   ((s-matches? (rx "You should not use punctuation in front of quotes.")
		msg)
    (attrap-one-option 'swap-punctuation-with-quotes
		       (progn
			 (forward-char)
			 (delete-char 2)
			 (backward-char)
			 (insert "''"))))))

(use-package emacs
  :ensure nil
  :after flycheck attrap
  :config
  (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer)))


(use-package attrap
  :ensure t
  :bind (("C-x /" . attrap-attrap))) ;; use any binding of your choice

(with-eval-after-load 'attrap
  (setcdr (assoc 'LaTeX-flymake attrap-flymake-backends-alist)
	  #'czm-attrap-LaTeX-fixer-flymake))

(defun czm/latex-tmp-new ()
  "Create new temporary LaTeX buffer."
  (interactive)
  (let ((dir (file-name-as-directory my-tmp-tex-dir))
	(filename (format-time-string "tmp-%Y%m%dT%H%M%S.tex")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((filepath (expand-file-name filename dir)))
      (find-file filepath)
      (save-buffer)
      ;; (czm-preview-timer-toggle)
      )))


(use-package preview-auto
  :ensure (preview-auto
	   :host github
	   :repo "ultronozm/preview-auto.el"
	   :depth nil)
  :after latex
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)
  (preview-LaTeX-command-replacements
   '(preview-LaTeX-disable-pdfoutput)))


(defun preview--skip-preamble-region (region-text region-offset)
  "Skip preamble for the sake of predumped formats.
Helper function of `TeX-region-create'.

If REGION-TEXT doesn't contain preamble, it returns nil.
Otherwise, it returns cons (ALTERED-TEXT . ALTERED-OFFSET) where
ALTERED-TEXT is REGION-TEXT without the preamble part and
ALTERED-OFFSET is REGION-OFFSET increased by the number of lines
of the preamble part of REGION-TEXT."
  (if (and TeX-header-end (string-match TeX-header-end region-text))
      (cons (substring region-text (match-end 0))
	    (with-temp-buffer
	      (insert (substring region-text 0 (match-end 0)))
	      (+ region-offset (TeX-current-offset))))))

(defun czm-copy-standard-tex-files ()
  "Copy standard TeX files to the current directory."
  (interactive)
  ;; ask the user if he really wants to copy files into the current directory
  (if (y-or-n-p (format "Copy standard TeX files to %s? " default-directory))
      (let ((files (list my-common-tex-file my-master-bib-file)))
	(dolist (file files)
	  (let ((source (expand-file-name file))
		(dest (expand-file-name (file-name-nondirectory file) default-directory)))
	    (copy-file source dest t))))
    (message "Aborted.")))

(defalias 'czm-setup-tex-file
  (kmacro "l t x SPC s-s s-p z C-n C-n C-c C-p C-a C-c C-p C-f"))

;; cdlatex
;; (use-package cdlatex
;;   :ensure t
;;   :hook (LaTeX-mode . cdlatex-mode)
;;   :hook (org-mode . org-cdlatex-mode)
;;   ;; TODO: check doom config for info on keybindings that
;;   ;; might conflict with yasnippet
;;   :config
;;   (setq cdlatex-sub-super-scripts-outside-math-mode nil ;; sometimes
;; 	cdlatex-simplify-sub-super-scripts nil))


;; taken from karthink
(use-package cdlatex
  :after latex
  :ensure t
  ;; :commands turn-on-cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab))
  :defines (cdlatex-math-symbol-prefix cdlatex-command-alist)
  :config
  (setq cdlatex-math-symbol-prefix ?\;)
  (define-key cdlatex-mode-map
              (cdlatex-get-kbd-vector cdlatex-math-symbol-prefix)
              #'cdlatex-math-symbol)
  (dolist (cmd '(("vc" "Insert \\vect{}" "\\vect{?}"
                  cdlatex-position-cursor nil nil t)
                 ("tfr" "Insert \\tfrac{}{}" "\\tfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("sfr" "Insert \\sfrac{}{}" "\\sfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("abs" "Insert \\abs{}" "\\abs{?}"
                  cdlatex-position-cursor nil nil t)
                 ("equ*" "Insert equation* env"
                  "\\begin{equation*}\n?\n\\end{equation*}"
                  cdlatex-position-cursor nil t nil)
                 ("sn*" "Insert section* env"
                  "\\section*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("ss*" "Insert subsection* env"
                  "\\subsection*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("sss*" "Insert subsubsection* env"
                  "\\subsubsection*{?}"
                  cdlatex-position-cursor nil t nil)))
    (push cmd cdlatex-command-alist))

  (setq cdlatex-env-alist
        '(("align" "\\begin{align}
?
\\end{align}" "\\\\AUTOLABEL
?")
          ("equation" "\\begin{equation}
?
\\end{equation}" nil)))
  
  (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                    (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                                    (?. ("\\cdot" "\\circ"))
                                    (?6 ("\\partial"))
                                    (?v ("\\vee" "\\forall"))
                                    (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
  (setq cdlatex-math-modify-alist '((?k "\\mathfrak" "\\textfrak" t nil nil)
                                    (?b "\\mathbf" "\\textbf" t nil nil)
                                    (?B "\\mathbb" "\\textbf" t nil nil)
                                    (?t "\\text" nil t nil nil)))
  (setq cdlatex-paired-parens "$[{(")
  (cdlatex-reset-mode))
;; auctex-latexmk
(use-package auctex-latexmk
  :ensure t
  :hook ((LaTeX-mode . electric-pair-mode)
	 (LaTeX-mode . my/latex-with-outline))
  :mode ("\\.tex\\'" . latex-mode)
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default.
  ;; taken from:
  ;; https://github.com/tom-tan/auctex-latexmk/issues/33#issuecomment-939427447
  (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "LatexMk")))
  :config
  ;; outline for latex
  ;; from https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-latex.el#L31C1-L35C3
  (defun my/latex-with-outline ()
    (add-to-list 'minor-mode-overriding-map-alist
		 `(outline-minor-mode . ,outline-minor-mode-map))
    (outline-minor-mode 1))
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package embrace
  :ensure t
  :bind (:map TeX-mode-map
	      ("M-s a" . embrace-add)
	      ("M-s c" . embrace-change)
	      ("M-s d" . embrace-delete)))



(use-package overleaf
  :ensure t
  ;; :config
  ;; 
  ;; ;; ;; Example: load cookies from firefox
  ;; ;; (setq overleaf-cookies
  ;; ;;       (overleaf-read-cookies-from-firefox "~/.mozilla/firefox/[YOUR PROFILE].default/cookies.sqlite")))
  ;; 
  ;; ;; Example: load/save cookies from GPG encrypted file.
  ;; ;;          (remove the .gpg extension to save unencrypted)
  ;; (let ((cookie-file "~/.overleaf-cookies.gpg"))
  ;;   (setq overleaf-save-cookies
  ;;         (overleaf-save-cookies-to-file cookie-file))
  ;;   (setq overleaf-cookies
  ;;         (overleaf-read-cookies-from-file cookie-file))))
  :custom
  (overleaf-use-nerdfont t "Use nerfont icons for the modeline."))


;; texfrag - preview latex segments in non-latex buffers
(use-package texfrag
  :ensure (texfrag
	   :host github
	   :repo "TobiasZawada/texfrag")
  :defer t)



;;; Writeroom mode
(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100)
  (setq-local fill-column 100))

(provide 'setup-latex)
