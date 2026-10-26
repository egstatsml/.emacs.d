;;; setup-elfeed.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'cl-lib)
(require 'url)
(require 'dom)

(defvar my-webfeeds-state-file
  (expand-file-name "webfeeds-seen.el" user-emacs-directory))

(defvar my-webfeeds-sites
  '((:name "Hidden Phenomena"
           :url "https://hidden-phenomena.com"
           :tags (hidden-phenomena maths)
           :extractor my-webfeeds-hidden-phenomena))
  "List of website definitions.

:name       Human-readable name.
:url        Website URL.
:tags       Elfeed tags.
:extractor  Function returning a list of entries.

Extractor functions must return:

  ((:title \"Title\"
    :url \"https://example.com/article\")

   ...)")

(defun my-webfeeds-load-state ()
  (if (file-exists-p my-webfeeds-state-file)
      (with-temp-buffer
        (insert-file-contents my-webfeeds-state-file)
        (read (current-buffer)))
    (make-hash-table :test #'equal)))

(defun my-webfeeds-save-state (state)
  (with-temp-file my-webfeeds-state-file
    (prin1 state (current-buffer))))

(defun my-webfeeds-fetch-dom (url)
  (with-current-buffer
      (url-retrieve-synchronously url t t 30)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (prog1
        (libxml-parse-html-region (point) (point-max))
      (kill-buffer))))

(defun my-webfeeds-add-entry (title url tags)
  (with-elfeed-db-visit (entry url)
    (setf (elfeed-entry-title entry) title)
    (setf (elfeed-entry-date entry) (float-time))
    (elfeed-tag entry tags)
    (elfeed-db-add entry)))

(defun my-webfeeds-update-site (site state)
  (let* ((name      (plist-get site :name))
         (tags      (plist-get site :tags))
         (extractor (plist-get site :extractor))
         (entries   (funcall extractor))
         (count     0))

    (dolist (item entries)
      (let ((url   (plist-get item :url))
            (title (plist-get item :title)))

        (unless (gethash url state)
          (puthash url t state)
          (my-webfeeds-add-entry title url tags)
          (cl-incf count))))
    (message "%s: %d new article(s)" name count)
    count))

(defun my-webfeeds-update ()
  "Update all configured websites."
  (interactive)
  (let ((state (my-webfeeds-load-state))
        (total 0))
    (dolist (site my-webfeeds-sites)
      (cl-incf total
               (my-webfeeds-update-site site state)))
    (my-webfeeds-save-state state)
    (elfeed-db-save)
    (message "Webfeeds: %d new article(s) total"
             total)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Site-specific extractors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-webfeeds-hidden-phenomena ()
  (let* ((base-url "https://hidden-phenomena.com")
         (dom (my-webfeeds-fetch-dom base-url))
         results)
    (dolist (node (dom-by-tag dom 'a))
      (let ((href (dom-attr node 'href))
            (title (string-trim (dom-texts node))))
        (when (and href
                   (string-match-p "^/articles/" href)
                   (not (string-empty-p title)))
          (push
           (list
            :title title
            :url (concat base-url href))
           results))))
    (delete-dups results)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example additional site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list
;;  'my-webfeeds-sites
;;  '(:name "Some Blog"
;;    :url "https://example.com"
;;    :tags (blog)
;;    :extractor my-webfeeds-example))
;;
;; (defun my-webfeeds-example ()
;;   ...)
;;
;; Then run:
;;
;;   M-x my-webfeeds-update
;;
;; Optional automatic refresh every 6 hours:
;;
;; (run-at-time nil (* 6 60 60) #'my-webfeeds-update)



(use-package elfeed
  :ensure t
  :if my/graphical
  :commands elfeed
  :config
  (add-hook 'elfeed-search-mode-hook 'elfeed-update)
  (defun my-search-print-fn (entry)
    (defun concatenate-authors (authors-list)
      "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
      (if (> (length authors-list) 1)
          (format "%s et al." (plist-get (nth 0 authors-list) :name))
        (plist-get (nth 0 authors-list) :name)))

    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title)
                      (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (entry-authors (concatenate-authors
                           (elfeed-meta entry :authors)))
           (title-width (- (window-width) 10
                           elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title 100
                          :left))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face
                                              'elfeed-search-tag-face))
                      tags ","))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))

           (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
           (authors-column (elfeed-format-column entry-authors 40 :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")

      (insert (propertize title-column
                          'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
                          'kbd-help entry-authors) " ")
      (insert entry-score " ")
      (insert tags-str " ")
      ))

  (setq elfeed-search-print-entry-function #'elfeed-search-print-entry--default) ;; #'my-search-print-fn)
  (setq elfeed-search-date-format '("%y-%m-%d" 10 :left))
  (setq elfeed-search-title-max-width 110)
  (setq elfeed-feeds '(("http://export.arxiv.org/api/query?search_query=cat:stat.TH&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" STAT.TH)
                       ("http://export.arxiv.org/api/query?search_query=cat:stat.ML&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" STAT.ML)
                       ("http://export.arxiv.org/api/query?search_query=cat:stat.CO&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" STAT.COMP)
                       ("http://export.arxiv.org/api/query?search_query=cat:math.PR&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" MATH.PROB)
                       ("http://export.arxiv.org/api/query?search_query=cat:math.ST&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" MATH.STAT)
                       ("http://export.arxiv.org/api/query?search_query=cat:cs.AI&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" CS.AI)
                       ("http://export.arxiv.org/api/query?search_query=cat:cs.CV&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" CS.CV)
                       ("http://export.arxiv.org/api/query?search_query=cat:cs.IT&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" CS.IT)
                       ("http://export.arxiv.org/api/query?search_query=cat:cs.RO&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" CS.ROB)
                       ("https://protesilaos.com/codelog.xml" EMACS)
                       ("https://protesilaos.com/news.xml" PROT)
                       ("https://protesilaos.com/books.xml" PROT)
                       ("https://protesilaos.com/politics.xml" PROT)
                       ("https://protesilaos.com/commentary.xml" PROT)
                       ("https://protesilaos.com/news.xml" EMACS)
                       ("https://karthinks.com/index.xml" EMACS)
                       ("https://hidden-phenomena.com/index.xml" MATH)
                       ))
  ;; add my custom sites
  (setq elfeed-search-filter "@2-week-ago +unread")
  ;; adding some custom tags
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "arxiv\\.org"
				:add '(arxiv)))
  (defun my/elfeed-entry-to-arxiv ()
    "Fetch an arXiv paper into the local library from the current elfeed entry.

  This is a customized version from the one in https://gist.github.com/rka97/57779810d3664f41b0ed68a855fcab54
  New features to this version:

  - Update the bib entry with the pdf file location
  - Add a TODO entry in my papers.org to read the paper
  "
    (interactive)
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" link))
           (matched-arxiv-number (match-string 1 link))
           (last-arxiv-key "")
           (last-arxiv-title ""))
      (when matched-arxiv-number
        (message "Going to arXiv: %s" matched-arxiv-number)
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
          (find-file (concat org-directory "/papers.org"))
          (goto-char (point-max))
          (insert (format "** TODO Read paper (cite:%s) %s" last-arxiv-key last-arxiv-title))
          (save-buffer)))))

  (setq arxiv_bib ethan/main-bib-library)
  (setq arxiv_pdf_loc ethan/main-pdfs-library-path)
  (setq org-ref-pdf-directory arxiv_pdf_loc))

(use-package elfeed-score
  :ensure t
  :if my/graphical
  :after elfeed
  :config
  (setq elfeed-score-serde-score-file (concat user-emacs-directory "elfeed.score"))
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))

(provide 'setup-elfeed)
