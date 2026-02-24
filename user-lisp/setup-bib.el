;;; setup-bib.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;;
;;;


;;; bibliographies
(use-package citar
  :ensure t
  :if my/graphical
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :bind (:map org-mode-map
	      ("C-; C-c c" . citar-insert-citation)
	      ;; for latex need to load specify the package "latex"
	      ;; which builds auctex for me in my current config as
	      ;; LaTeX-mode-map is not autoloaded
	      :map LaTeX-mode-map
	      :package latex
	      ("C-; C-c c" . citar-insert-citation))
  :config
  ;; setting citar to handle all citations
  (setq org-cite-insert-processor 'citar
	org-cite-follow-processor 'citar
	org-cite-activate-processor 'citar)
  ;; setting paths for citar bib files and pdf directories
  (setq citar-bibliography ethan/bib-libraries
	citar-library-paths ethan/main-pdfs-library-paths
	citar-file-extensions '("pdf" "org" "md")
	citar-file-open-function #'find-file)
  (defun ethan/citar-full-names (names)
    "Transform names like LastName, FirstName to FirstName LastName."
    (when (stringp names)
      (mapconcat
       (lambda (name)
	 (if (eq 1 (length name))
	     (split-string name " ")
	   (let ((split-name (split-string name ", ")))
	     (cl-concatenate 'string (nth 1 split-name) " " (nth 0 split-name)))))
       (split-string names " and ") ", ")))
  (setq citar-display-transform-functions
	'(
	  (("author" "editor") . ethan/citar-full-names)))
  (setq citar-templates
	'((main . "${author editor:55}     ${date year issued:4}     ${title:55}")
	  (suffix . "  ${tags keywords keywords:40}")
	  (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
	  (note . "#+title: Notes on ${author editor}, ${title}")))
  ;; nerd icons in citar
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
	      "nf-fa-file_o"
	      :face 'nerd-icons-green
	      :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
	      "nf-fa-link"
	      :face 'nerd-icons-orange
	      :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
	      "nf-cod-note"
	      :face 'nerd-icons-blue
	      :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
	      "nf-fa-circle_o"
	      :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
	(list citar-indicator-files-icons
	      citar-indicator-links-icons
	      citar-indicator-notes-icons
	      citar-indicator-cited-icons)))


(use-package citar-embark
  :if my/graphical
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))


(use-package citar-org-node
  :ensure (citar-org-node
           :host github
           :repo "krisbalintona/citar-org-node")
  :after (:any citar org-node)
  :bind ( :map org-mode-map
          ("C-c b a" . citar-org-node-add-refs)
          ("C-c b o" . citar-org-node-open-resource))
  :config
  (citar-org-node-mode 1))

;; I use org-ref really for just two formating bibtex entries
(use-package org-ref
  :ensure t
  :if my/graphical
  :commands (arxiv-get-pdf-add-bibtex-entry arxiv-get-pdf)
  :config
  (setq library-download-directory ethan/main-pdfs-library-path)
  (defun ethan/reformat-bib-library (&optional filename)
    "Formats the bibliography using biber & rebiber and updates the PDF -metadata."
    (interactive "P")
    (or filename (setq filename ethan/main-bib-library))
    (let ((cmnd (concat
		 (format "rebiber -i %s &&" filename) ; Get converence versions of arXiv papers
		 (format "biber --tool --output_align --output_indent=2 --output_fieldcase=lower --configfile=~/bib-lib/biber-myconf.conf --output_file=%s %s && " filename filename) ; Properly format the bibliography
		 (format "sed -i -e 's/arxiv/arXiv/gI' -e 's/journaltitle/journal     /' -e 's/date      /year      /' %s &&" filename) ; Some replacements
		 )))
      (async-shell-command cmnd)))
  (defun ethan/reformat-bib-lib-hook ()
    "Reformat the main bib library whenever it is saved.."
    (when (equal (buffer-file-name) ethan/main-bib-library) (ethan/reformat-bib-library)))
  (add-hook 'after-save-hook 'ethan/reformat-bib-lib-hook)
  (setq bibtex-dialect 'biblatex)
  ;; settings for automatically getting citekeys in the format I want
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5
	org-ref-bibtex-hydra-key-binding (kbd "H-b"))
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body))

(use-package zotra
  ;; check out the zotra and zotra server package for info on getting started
  ;; https://github.com/mpedramfar/zotra-server
  :ensure t
  :if my/graphical
  :defer 1
  :init
  :config
  (setq zotra-backend 'citoid)
  (setq zotra-backend 'translation-server)
  (setq zotra-local-server-directory "~/code/zotra-server"))


;;; setup org protocol for my bib access and saving files from arxiv
;; copied a lot from
;; https://koustuvsinha.com/post/emacs_org_protocol_arxiv/
(defun my/save-arxiv-to-local-db (matched-arxiv-number)
  "Save arxiv paper in local db

- Update the bib entry with the pdf file location
- Add a TODO entry in my papers.org to read the paper"
  (message "Going to arXiv: %s" matched-arxiv-number)
  (let* ((last-arxiv-key "")
	 (last-arxiv-title ""))
    (arxiv-get-pdf-add-bibtex-entry matched-arxiv-number arxiv_bib arxiv_pdf_loc)
    ;; Now, we are updating the most recent bib file with the pdf location
    (message "Update bibtex with pdf file location")
    (save-window-excursion
      ;; Get the bib file
      (find-file arxiv_bib)
      ;; get to last line
      (goto-char (point-max))
      ;; get to the first line of bibtex
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry))
	     (key (cdr (assoc "=key=" entry)))
	     (title (bibtex-completion-apa-get-value "title" entry))
	     (pdf (org-ref-get-pdf-filename key)))
	(message (concat "checking for key: " key))
	(message (concat "value of pdf: " pdf))
	(when (file-exists-p pdf)
	  (bibtex-set-field "file" pdf)
	  (setq last-arxiv-key key)
	  (setq last-arxiv-title title)
	  (save-buffer)
	  )))
    ;; (message (concat "outside of save window, key: " last-arxiv-key))
    ;; Add a TODO entry with the cite key and title
    ;; This is a bit hacky solution as I don't know how to add the org entry programmatically
    (save-window-excursion
      (find-file (concat org-directory "/reading.org"))
      (goto-char (point-max))
      (insert (format "** TOREAD Read paper (cite:%s) %s" last-arxiv-key last-arxiv-title))
      (save-buffer))))

(with-eval-after-load 'org-protocol
  (when (daemonp)
    (progn
      ;; org-protocol-protocol-alist is only available in daemon/server mode
      (add-to-list 'org-protocol-protocol-alist
		   '("arxiv-protocol"
		     :protocol "arxiv"
		     :function arxiv-protocol))
      ;; function that gets called for arxiv-protocol
      (defun arxiv-protocol (info)
	(let ((url (plist-get info :url)))
	  (message (format "Arxiv received: `%s'" url))
	  (let* ((match-idx (string-match "arxiv.org/.../\\([0-9.]*\\)" url))
		 (matched-arxiv-number (string-remove-suffix "." (match-string 1 url))))
	    (message (format "Extracted Arxiv number: `%s'" matched-arxiv-number))
	    (when matched-arxiv-number
	      (my/save-arxiv-to-local-db matched-arxiv-number)))
	  nil)))))

(provide 'setup-bib)
