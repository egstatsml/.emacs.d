
(defun dw/org-mode-setup ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defun my/org-todo-done ()
  (interactive)
  (let ((state (org-get-todo-state))
        post-command-hook)
    (if (not(string= state "DONE"))
        (org-todo "DONE")
      (org-todo "TODO"))
    (run-hooks 'post-command-hook)
    (org-flag-subtree t)))


(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :custom-face
  (variable-pitch ((t (:family "ETBembo"))))
  ;;(variable-pitch ((t (:family "Avenir Next" :height 160 :weight light))))
  ;;    (fixed-pitch ((t (:family "Inconsolata Nerd Font"))))
  (fixed-pitch ((t (:family "FiraCode Nerd Font Mono" ))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-done ((t (:foreground "PaleGreen"
                 :strike-through t))))
  :bind
  ("C-c d" . 'my/org-todo-done)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-modules
    '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  ;; seting sizes for things and fonts
  (let* ((variable-tuple
          (cond ((x-list-fonts   "ETBembo")         '(:font   "ETBembo"))
                ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
                ((x-list-fonts   "Lucida Grande")   '(:font   "Lucida Grande"))
                ((x-list-fonts   "Verdana")         '(:font   "Verdana"))
                ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font."))))
         (base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default
                              :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8        ((t (,@headline ,@variable-tuple))))
     `(org-level-7        ((t (,@headline ,@variable-tuple))))
     `(org-level-6        ((t (,@headline ,@variable-tuple))))
     `(org-level-5        ((t (,@headline ,@variable-tuple))))
     `(org-level-4        ((t (,@headline ,@variable-tuple))))
     `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.0))))
     `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.0))))
     `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
     `(org-document-title ((t (,@headline ,@variable-tuple
                                          :height 1.4 :underline nil)))))))

;; adding org-bullets and setting it to indent-mode
(use-package org-bullets
  :ensure t
  :after (org)
  :hook (org-mode . org-bullets-mode))


;;org-wiki mode
;; add timestamp when closing a task
(setq org-log-done 'time)
;; (require 'org-wiki)
;; (require 'helm-config)
;;
;;turn line numbers off
(defun nolinum ()
  (linum-mode 0))
(add-hook 'org-mode-hook 'nolinum)
;; adding org-bullets and setting it to indent-mode
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
;; ;
                                        ;
(setq org-auto-align-tags 1)
(setq org-wiki-location-list
      '(
        "~/org/wiki"    ;; First wiki (root directory) is the default.
        "~/org/blog"
        ))
;;forcing image size if it is too large
(setq org-image-actual-width '(600))
;; Initialize first org-wiki-directory or default org-wiki
(setq org-wiki-location (car org-wiki-location-list))


;; setting up faster access t GTD file
(defun ethan/open-gtd-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (org-open-file "~/org/wiki/gtd.org"))
(global-set-key (kbd "C-c g") 'ethan/open-gtd-file)

;;publishing options

(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("org-notes"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/org/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        ("org-static"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org"
         :components ("org-notes" "org-static")
         )
        ))
;;then can publish with M-x org-publish-project RET org RET

;; setting org tags
(setq org-tag-alist
      '(("PHD")
        ("LIFE")
        ("PROG")
        ("WRITE")
        ("UPDATE")
        ("MEET")
        ("PRESENT")
        ("DEADLINE")
        ("CLOUDFORGE")
        ("HEALTH")
        ("PROJECT")
        ("HABIT")))

;; org temoplate expansion
(add-to-list 'org-structure-template-alist '("r" . "src R"))
(add-to-list 'org-structure-template-alist '("p" . "src python"))


;;now setting up some alias commands for org-wiki mode
(defalias 'w-i #'org-wiki-index)
(defalias 'w-in #'org-wiki-insert-new)
(defalias 'w-l #'org-wiki-latex)
(defalias 'w-lin #'org-wiki-insert-latex)
(defalias 'w-f #'org-wiki-helm)
(defalias 'w-e #'org-wiki-export-html) ;;exports the entire (well most) of the wiki
(defalias 'w-s #'org-wiki-switch-root);;switch wiki
(defalias 'w-h #'org-html-export-to-html);;exports a single page
(defalias 'w-im #'org-display-inline-images)
(defalias 'w-p #'org-publish-project);;to publish to HTML
(defalias 'w-b #'my-insert-bibtex);;use for inserting bibtex entries
(defalias 'w-t #'my-insert-template);;use for inserting reading notes template

;;automatically change parent task to DONE when all child tasks are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;export checkboxes with pretty prettiness in html
(setq org-html-checkbox-type 'html)

;; enable org-ref and bibtext with export
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;;add some more TODO list keywords
(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "INPROGRESS" "|" "DONE")
        (sequnce "TOREAD" "|" "DONE")
        (sequence "INPROGRESS" "FEEDBACK" "WAITING" "VERIFY" "|" "DONE")))

;; some self defined functions for LaTeX environments
(defun my-insert-equation ()
  (interactive)
  (insert "\\begin{equation*} \n\n\\end{equation*}")
  (previous-line)
    (insert "    ")
  )
(defun my-insert-align ()
  (interactive)
  (insert "\\begin{align*} \n\n\\end{align*}")
  (previous-line)
    (insert "    ")
  )
(defun my-insert-gather ()
  (interactive)
  (insert "\\begin{gather*} \n\n\\end{gather*}")
  (previous-line)
  (insert "    ")
  )

(defun my-add-effort-property (time_string)
  "Add EFFORT property to add to org-mode TODO task"
  (interactive "sEstimated Time (H:M): ")
  (org-set-property "EFFORT" time_string))

(defun my-add-habit-property ()
  "Add EFFORT property to add to org-mode TODO task"
  (interactive)
  (org-set-property "STYLE" "habit"))

(defun my-org-archive-done-tasks ()
  "archives done tasks in subtree

   Got from https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command"
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;;setting up global key-bindings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

;;setting up my local key-bindings
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-e") 'my-insert-equation)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-a") 'my-insert-align)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-g") 'my-insert-gather)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-p") 'org-preview-latex-fragment)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c i") 'org-ref-insert-cite-with-completion)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c e") 'my-add-effort-property)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c h") 'my-add-habit-property)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c s") 'my-org-archive-done-tasks)))


;; refile targets
(setq org-refile-targets '(("~/org/wiki/gtd.org" :maxlevel . 1)
                           ("~/org/wiki/someday.org" :maxlevel . 1)
                           ("~/org/wiki/tickler.org" :maxlevel . 1)))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (ein . t)
   (emacs-lisp . nil)))

;; hide emhpasis markers (such as /../ for italics and *...* for BOLD
(setq org-hide-emphasis-markers t)

(defun end-of-month-fn ()
  (-let* (((sec minute hour day month year dow dst utcoff) (decode-time))
          (last-day-of-month (calendar-last-day-of-month month year)))
           ;; A hack that seems to work fine.  Yay, Postel!
           (format "%d-%02d-%02d" year month last-day-of-month)
    ))


(defun seven-days-time-fn ()
  (-let* ((current-time (decode-time))
          (delta (make-decoded-time :day 6))
          ((sec minute hour day month year dow dst utcoff) (decoded-time-add current-time delta)))
    ;; A hack that seems to work fine.  Yay, Postel!
     (format "%d-%02d-%02d" year month day)
    ))

(setq seven-days-time (seven-days-time-fn))
(setq end-of-month (end-of-month-fn))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-additional-timestamps-same-entry t
      org-agenda-skip-timestamp-if-done t
      org-habit-show-habits-only-for-today t
      org-agenda-view-columns-initially nil)

;; adding evil org mode so that I can use evil keys within org agenda specifically
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;;default org-modules = (ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
(setq org-modules
      (quote
       (ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-habit)))


;; setting the default column views
(setq org-columns-default-format "#+COLUMNS: %38ITEM(Details) %PRIORITY(Priority) %TAGS(Context) %7TODO(To Do) %5Effort(Estimated Time){:} %CLOCKSUM(Logged Time) ")

;; org roam
(use-package org-roam
  :ensure t
  :init
  ;; directory for my journal
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/wiki/roam"))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n* Exercise\n\n* Meditation\n\n* What I learn't today\n\n* Question of the Day\n\n* Something nice that happened\n\n* Notes
"))))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("c" "custom" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+TAGS: PHD LIFE PROG WRITE UPDATE MEET PRESENT DEADLINE CLOUDFORGE HABIT")
           :unnarrowed t)
          ("r" "bibliography reference" plain
           (file "~/.emacs.d/noter_template.org")
           :if-new
           (file+head "references/${citekey}.org" "#+title: ${title}\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ;;("C-c n j" . org-roam-dailies-capture-today))
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  ;;(org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; load config for agenda
(load-file "~/.emacs.d/org_agenda.el")

(use-package org-ref
  ;; :init
  ;; code to run before loading org-ref
  :config
  (setq bibtex-completion-bibliography ethan/bibliography-path
	bibtex-completion-library-path ethan/pdf-path
        bibtex-completion-notes-path ethan/bibliography-notes
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	;; bibtex-completion-pdf-open-function
	;; (lambda (fpath)
	;; (call-process "open" nil 0 nil fpath)))
        )
  (setq
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "#+ROAM_TAGS: ${keywords}\n"
    "#+filetags:REFERENCE\n"
    "#+CREATED:%<%Y-%m-%d-%H-%M-%S>\n"
    "Time-stamp: <>\n"
    "- tags :: \n"
    "* NOTES \n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n"
    )))


;; (use-package org-ref
;;     ;; :init
;;     ; code to run before loading org-ref
;;     :config
;;     (setq
;;      org-ref-notes-function 'org-ref-notes-function-many-files
;;      org-ref-completion-library 'org-ref-ivy-cite
;;      org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
;;      org-ref-default-bibliography (list "/home/ethan/.emacs.d/ref.bib")
;;      org-ref-bibliography-notes "~/org/wiki/bibnotes.org"
;;      org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
;;      org-ref-notes-directory ethan/bibliography-notes
;;      org-ref-notes-function 'orb-edit-notes
;;      ))

;; (setq
;;  bibtex-completion-notes-path ethan/bibliography-notes
;;  bibtex-completion-bibliography "~/.emacs.d/ref.bib"
;;  bibtex-completion-pdf-field "file"
;;  bibtex-completion-notes-template-multiple-files
;;  (concat
;;   "#+TITLE: ${title}\n"
;;   "#+ROAM_KEY: cite:${=key=}\n"
;;   "#+ROAM_TAGS: ${keywords}\n"
;;   "#+filetags:REFERENCE\n"
;;   "#+CREATED:%<%Y-%m-%d-%H-%M-%S>\n"
;;   "Time-stamp: <>\n"
;;   "- tags :: \n"
;;   "* NOTES \n"
;;   ":PROPERTIES:\n"
;;   ":Custom_ID: ${=key=}\n"
;;   ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
;;   ":AUTHOR: ${author-abbrev}\n"
;;   ":JOURNAL: ${journaltitle}\n"
;;   ":DATE: ${date}\n"
;;   ":YEAR: ${year}\n"
;;   ":DOI: ${doi}\n"
;;   ":URL: ${url}\n"
;;   ":END:\n\n"
;;   ))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind
  ("C-c b a" . orb-note-actions)
  ("C-c b i" . orb-insert-link)
  :config
  (setq orb-note-actions-interface 'ivy)
  (setq orb-insert-interface 'ivy-bibtex)
  (setq orb-insert-generic-candidates-format 'ivy-bibtex)
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;;org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   ;;org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org-noter-notes-search-path '("~/org/wiki/roam/references/");;ethan/bibliography-notes
   )
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))
  (require 'org-noter-pdftools))
