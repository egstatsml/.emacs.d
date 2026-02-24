(use-package library
  :if my/graphical
  :defer t
  :ensure (library
	   :host github
	   :repo "ultronozm/library.el")
  :commands (library-download-arxiv-no-open)
  :custom
  (library-pdf-directory ethan/main-pdfs-library-path)
  (library-bibtex-file ethan/main-bib-library)
  (library-download-directory "~/Downloads/")
  (library-local-pdf-link-name "pdf link")
  (library-org-capture-template-key "j")
  :config
  ;; must set bibtex dialect
  (bibtex-set-dialect)
  (defun library-download-arxiv-no-open (id)
    "Download, process arxiv file but do not open it.
  When called interactively, defaults to URL at point if present."
    (interactive
     (list (read-string "arXiv ID: " (thing-at-point 'url))))
    (save-excursion
      (when (string-match ".*/" id)
	(setq id (substring id (match-end 0))))
      (when (string-match "\\.pdf$" id)
	(setq id (substring id 0 -4)))
      (let* ((id (library--ensure-utf8-encoding id))
             (url (format "https://arxiv.org/pdf/%s.pdf" id))
             (outfile (expand-file-name
                       (format "%s.pdf" id) library-download-directory)))
	(url-copy-file url outfile t)
	(if (file-exists-p outfile)
            (when-let (newfile (library-process-arxiv outfile))
              (message "Downloaded pdf to %s" newfile))
          (message "File not found.")))))


  (defun my/library--generate-filename (_entry year author title)
    "Generate filename from bibtex ENTRY, YEAR, AUTHOR, and TITLE.

I want the filenames to match what google-scholar does.
"
    (message "manual author: %s" (bibtex-text-in-field "author" _entry))
    (message "author: %s" author)
    (let* ((my-author (bibtex-text-in-field "author" _entry))
	   (my-title (bibtex-text-in-field "title" _entry))
	   ;; this concatenates first and last names when using arxiv
	   ;; API bibtex entries.  could optimize it to just use last
	   ;; names, but this is fine for now.
	   (lastname
	    (downcase
	     ;; get the first authors last name
	     (car (last (split-string
			 ;; split authors and return first one
			 (car (split-string my-author (rx (seq (zero-or-more space)
							       word-boundary (group (or "and" ","))
							       word-boundary (zero-or-more space))))))))))
	   (clean-title
	    (downcase
	     (car ;; get the first word, but filter out small words
	      (cl-remove-if (lambda (str) (<= (length str) 3))
			    (split-string
			     my-title))))))
      (concat lastname year clean-title)))


  (defun my/insert-validate-bibtex-entry (bibtex)
    (insert bibtex)
    (goto-char (point-min))
    (search-forward-regexp "{\\([^,]+\\)")
    (setq name (match-string 1))
    (if (not name)
	(error "Invalid bib entry")))

  (defun my/extract-key (bibtex)
    (with-temp-buffer
      (my/insert-validate-bibtex-entry bibtex)
      (library--filename-from-bibtex)))


  (defun my/rename-bib-key (key &optional allow-duplicate-keys)
    "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file, unless ALLOW-DUPLICATE-KEYS is non-nil.

Taken from org-ref"
    (goto-char (point-max))
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (and (not allow-duplicate-keys)
	       (save-excursion
		 (bibtex-search-entry key)))
      (save-excursion
	(bibtex-search-entry key)
	(bibtex-copy-entry-as-kill)
	(switch-to-buffer-other-window "*duplicate entry*")
	(bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
    (insert key)
    (kill-new key))


  (defun library--process-pdf-bibtex (file bibtex)
    "Process pdf FILE with associated BIBTEX.
First, deposit BIBTEX into references file.  Then, move PDF file
to PDF directory, and rename it according to the BIBTEX entry."
    (interactive)
    (when-let* ((key (my/extract-key bibtex))
		(path (library--path-of-name key)))
      (library--deposit-bibtex-filename-key bibtex key path)
      (dired-rename-file file path t)
      path))

  (defun library--deposit-bibtex-filename-key (bibtex key path)
    "Deposit BIBTEX into references file, and set the key."
    (save-window-excursion
      (let ((name))
	(with-temp-buffer
	  (insert bibtex)
	  (goto-char (point-min))
	  (search-forward-regexp "{\\([^,]+\\)")
	  (setq name (match-string 1))
	  (if (not name)
	      (error "Invalid bib entry")))
	;; (insert-validate-bibtex-entry bibtex))
	(find-file library-bibtex-file)
	(goto-char (point-min))
	(unless (search-forward name (point-max) t)
	  (goto-char (point-max))
	  (newline-and-indent)
	  (insert bibtex)
	  (bibtex-set-field "file" path)
	  ;; use the filename as a key
	  (my/rename-bib-key key)
	  (save-buffer)))))

  (setq library-generate-filename-function 'my/library--generate-filename)

  (with-eval-after-load 'org
    (add-to-list 'org-capture-templates
		 '("j" "Journal" entry (file "~/org/reading.org")))))
