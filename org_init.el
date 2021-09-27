
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :custom-face
  (variable-pitch ((t (:family "ETBembo"))))
  ;;(variable-pitch ((t (:family "Avenir Next" :height 160 :weight light))))
  ;;    (fixed-pitch ((t (:family "Inconsolata Nerd Font"))))
  (fixed-pitch ((t (:family "Fira Code Retina" ))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-done ((t (:foreground "PaleGreen"
                 :strike-through t))))
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
     `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.4))))
     `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
     `(org-document-title ((t (,@headline ,@variable-tuple
                                          :height 1.75 :underline nil))))))
 ;; babel languages
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (ledger . t))))




;;org-wiki mode
;; add timestamp when closing a task
(setq org-log-done 'time)
(require 'org-wiki)
(require 'helm-config)
;;turn line numbers off
(defun nolinum ()
  (linum-mode 0))
(add-hook 'org-mode-hook 'nolinum)
;; adding org-bullets and setting it to indent-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
;;
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
      '((sequence "TODO" "NEXT" "|" "DONE")
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



;; some pretty options added from
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/

;; hide emhpasis markers (such as /../ for italics and *...* for BOLD
(setq org-hide-emphasis-markers t)
;; ;; font sizes for headings
;; (let*
;;     ((variable-tuple
;;       (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;             ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;             ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;      (base-font-color     (face-foreground 'default nil 'default))
;;      (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05))))
;;    `(org-level-2 ((t (,@variable-tuple))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; ;; setting custom pitch faces
;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 100 :weight thin))))
;;  '(fixed-pitch ((t ( :family "Source Code Pro Regular" :height 100)))))

;; ;; makes sure fill long lines works well with proportional fonts
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; ;; making it so don't have proportional/variable font in key items
;; (custom-theme-set-faces
;;  'user
;;  '(org-block ((t (:inherit fixed-pitch))))
;;  '(org-code ((t (:inherit fixed-pitch))))
;;  '(org-document-info ((t (:inherit fixed-pitch))))
;;  '(org-document-info-keyword ((t (:inherit fixed-pitch))))
;;  '(org-indent((t (:inherit fixed-pitch))))
;;  '(org-link ((t (:inherit fixed-pitch))) t)
;;  '(org-meta-line ((t (:inherit fixed-pitch))))
;;  '(org-property-value ((t (:inherit fixed-pitch))))
;;  '(org-special-keyword ((t (:inherit fixed-pitch))))
;;  '(org-table ((t (:inherit fixed-pitch))))
;;  '(org-tag ((t (:inherit fixed-pitch))))
;;  '(org-verbatim((t (:inherit fixed-pitch)))))



 ;; '(org-block ((t (:inherit fixed-pitch))))
 ;; '(org-code ((t (:inherit (shadow fixed-pitch)))))
 ;; '(org-document-info ((t (:foreground "dark orange"))))
 ;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 ;; '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 ;; '(org-link ((t (:inherit fixed-pitch))) t)
 ;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 ;; '(org-property-value ((t (:inherit fixed-pitch))) t)
 ;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 ;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 ;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 ;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; old link property  '(org-link ((t (:foreground "royal blue" :underline t))))


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

;;default org-modules = (ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
(setq org-modules
      (quote
       (ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-habit org-temp)))


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


;; for agenda commands
;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
;; I got it from https://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
  If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backupfiles
         (filext (or filext "org$\\\|org_archive"))
         (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir)             ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (add-to-list 'org-file-list org-file)))))))
;; setting org agenda files
;; (setq org-agenda-files
;;       (sa-find-org-file-recursively "~/org/wiki/roam" "org"))



(defun my/org-agenda-todo-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file.
Taken from https://sachachua.com/blog/2013/01/emacs-org-task-related-keyboard-shortcuts-agenda/
and https://emacs.stackexchange.com/questions/19403/how-do-i-change-key-bindings-for-org-mode-agenda-view
"
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
;;(define-key org-agenda-mode-map "x" 'org-agenda-done)
(add-hook 'org-agenda-mode-hook
          (lambda ()
                  (local-set-key (kbd "d") 'my/org-agenda-todo-done)))

;;(bind-key "<apps> a" 'org-agenda)

(defun my/org-agenda-project-agenda ()
  "Return the project headline and up to `org-agenda-max-entries' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))

(defun my/org-agenda-projects-and-tasks (match)
  "Show TODOs for all `org-agenda-files' headlines matching MATCH."
  (interactive "MString: ")
  (let ((todo-only nil))
    (if org-agenda-overriding-arguments
        (setq todo-only (car org-agenda-overriding-arguments)
              match (nth 1 org-agenda-overriding-arguments)))
    (let* ((org-tags-match-list-sublevels
            org-tags-match-list-sublevels)
           (completion-ignore-case t)
           rtn rtnall files file pos matcher
           buffer)
      (when (and (stringp match) (not (string-match "\\S-" match)))
        (setq match nil))
      (when match
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher)))
      (catch 'exit
        (if org-agenda-sticky
            (setq org-agenda-buffer-name
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m")) match)
                    (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
        (org-agenda-prepare (concat "TAGS " match))
        (org-compile-prefix-format 'tags)
        (org-set-sorting-strategy 'tags)
        (setq org-agenda-query-string match)
        (setq org-agenda-redo-command
              (list 'org-tags-view `(quote ,todo-only)
                    (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
        (setq files (org-agenda-files nil 'ifmode)
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (setq buffer (if (file-exists-p file)
                             (org-get-agenda-file-buffer file)
                           (error "No such file %s" file)))
            (if (not buffer)
                ;; If file does not exist, error message to agenda
                (setq rtn (list
                           (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                      rtnall (append rtnall rtn))
              (with-current-buffer buffer
                (unless (derived-mode-p 'org-mode)
                  (error "Agenda file %s is not in `org-mode'" file))
                (save-excursion
                  (save-restriction
                    (if org-agenda-restrict
                        (narrow-to-region org-agenda-restrict-begin
                                          org-agenda-restrict-end)
                      (widen))
                    (setq rtn (org-scan-tags 'my/org-agenda-project-agenda matcher todo-only))
                    (setq rtnall (append rtnall rtn))))))))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert "Press `C-u r' to search again with new search string\n"))
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (mapconcat 'identity rtnall "\n") ""))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties (point-min) (point-max)
                             `(org-agenda-type tags
                                               org-last-args (,todo-only ,match)
                                               org-redo-cmd ,org-agenda-redo-command
                                               org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))


(defvar my/org-agenda-contexts
  '((tags-todo "phone")
    (tags-todo "work")
    (tags-todo "drawing")
    (tags-todo "coding")
    (tags-todo "writing")
    (tags-todo "computer")
    (tags-todo "home")
    (tags-todo "errands"))
  "Usual list of contexts.")
(defun my/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

;; (setq org-agenda-custom-commands
;;       `(("a" "Agenda"
;;          ((agenda "" ((org-agenda-span 2)))
;;           ;; Projects
;;           (tags "+PROJECT-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive-evilplans"
;;                 ((org-tags-exclude-from-inheritance '("PROJECT"))
;;                  (org-agenda-prefix-format "  ")
;;                  (org-agenda-overriding-header "Projects: ")
;;                  (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;           ;; Inbox
;;           (alltodo ""
;;                    ((org-agenda-prefix-format "%-6e ")
;;                     (org-agenda-files '("~/org/wiki/inbox.org"))
;;                     (org-agenda-overriding-header "Inbox: ")))
;;           (todo "WAITING-inactive"
;;                 ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;                  (org-agenda-prefix-format "%-6e ")
;;                  (org-agenda-overriding-header "Waiting: ")
;;                  (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;           ;; Unscheduled
;;           (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans"
;;                      ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;                       (org-agenda-prefix-format "%-6e ")
;;                       (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;                       (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;           ))
;;         ("e" "Emacs" (tags "emacs"))
;;         ("i" "Inbox" alltodo ""
;;          ((org-agenda-files '("~/org/wiki/inbox.org"))))
;;         ("t" tags-todo "-cooking"
;;          ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
;;         ("T" tags-todo "TODO=\"TODO\"-goal-routine-cooking-SCHEDULED={.+}" nil "~/cloud/agenda/nonroutine.html")
;;         ("f" tags-todo "focus-TODO=\"DONE\"-TODO=\"CANCELLED\"")
;;         ("x" "Column view" todo ""  ; Column view
;;          ((org-agenda-prefix-format "")
;;           (org-agenda-cmp-user-defined 'my/org-sort-agenda-items-todo)
;;           (org-agenda-view-columns-initially t)
;;           ))
;;         ;; Weekly review
;;         ("w" "Weekly review" agenda ""
;;          ((org-agenda-span 7)
;;           (org-agenda-log-mode 1)) "~/org/wiki/this-week.html")
;;         ("W" "Weekly review sans routines" agenda ""
;;          ((org-agenda-span 7)
;;           (org-agenda-log-mode 1)
;;           (org-agenda-tag-filter-preset '("-routine"))) "~/org/wiki/this-week-nonroutine.html")
;;         ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
;;         ("5" "Quick tasks" tags-todo "EFFORT>=\"0:05\"&EFFORT<=\"0:15\"")
;;         ("0" "Unestimated tasks" tags-todo "EFFORT=\"\"")
;;         ("gc" "Coding" tags-todo "@coding"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gw" "Writing" tags-todo "@writing"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gp" "Phone" tags-todo "@phone"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gd" "Drawing" tags-todo "@drawing"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gh" "Home" tags-todo "@home"
;;          ((org-agenda-view-columns-initially t)))
;;         ("ge" "Errands" tags-todo "errands"
;;          ((org-agenda-view-columns-initially t))
;;          ("~/cloud/agenda/errands.html"))
;;         ("c" "Top 3 by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-sorting-strategy '(priority-up effort-down))
;;           (org-agenda-max-entries 3)))
;;         ("C" "All by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-sorting-strategy '(priority-down effort-down))
;;           (org-agenda-max-entries nil)))
;;         ("9" "Unscheduled top 3 by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-sorting-strategy '(priority-down effort-down))
;;           (org-agenda-max-entries 3)))
;;         ("(" "All unscheduled by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-sorting-strategy '(priority-down effort-down))
;;           ))
;;         ("d" "Timeline for today" ((agenda "" ))
;;          ((org-agenda-ndays 1)
;;           (org-agenda-show-log t)
;;           (org-agenda-log-mode-items '(clock closed))
;;           (org-agenda-clockreport-mode t)
;;           (org-agenda-entry-types '())))
;;         ("." "Waiting for" todo "WAITING")
;;         ("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine"
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-view-columns-initially nil)
;;           (org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;           (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
;;         ("r" "Unscheduled, untagged tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine-evilplans-computer-writing-phone-sewing-home-errands-shopping"
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-view-columns-initially nil)
;;           (org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;           (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
;;         ("s" "Someday" tags-todo "TODO=\"SOMEDAY\""
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-view-columns-initially nil)
;;           (org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-overriding-header "Someday: ")
;;           (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
;;         ("U" "Unscheduled tasks outside projects" tags-todo "-project-cooking-routine"
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-tags-exclude-from-inheritance nil)
;;           (org-agenda-view-columns-initially nil)
;;           (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
;;         ("P" "By priority"
;;          ((tags-todo "+PRIORITY=\"A\"")
;;           (tags-todo "+PRIORITY=\"B\"")
;;           (tags-todo "+PRIORITY=\"\"")
;;           (tags-todo "+PRIORITY=\"C\""))
;;          ((org-agenda-prefix-format "%-10c %-10T %e ")
;;           (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;         ("pp" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive"
;;          ((org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;         ("p." tags "+project-TODO=\"DONE\""
;;          ((org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;         ("S" tags-todo "TODO=\"STARTED\"")
;;         ("C" "Cooking"
;;          ((tags "vegetables")
;;           (tags "chicken")
;;           (tags "beef")
;;           (tags "pork")
;;           (tags "other"))
;;          ((org-agenda-files '("~/orgzly/cooking.org"))
;;           (org-agenda-view-columns-initially t)
;;           (org-agenda-sorting-strategy '(scheduled-up time-down todo-state-up)))
;;          )
;;         ("8" "List projects with tasks" my/org-agenda-projects-and-tasks
;;          "+PROJECT"
;;          ((org-agenda-max-entries 3)))))

(setq org-agenda-files
      (list "~/org/wiki/gtd.org"
            "~/org/wiki/tickler.org"
            "~/org/wiki/someday.org"
            "~/org/wiki/inbox.org"))


;; make sure tags don't over run multiple lines in agenda view
(setq org-agenda-align-tags-to-column -100)
(setq org-habit-graph-column 60)
(setq bibtex-completion-library-path '("~/org/wiki/pdfs"))
(setq ethan/bibliography-path "~/.emacs.d/ref.bib")
(setq ethan/pdf-path  "~/org/wiki/pdfs/")
(setq ethan/bibliography-notes "~/org/wiki/roam/references/")


(use-package org-ref
    ;; :init
    ; code to run before loading org-ref
    :config
    (setq
     org-ref-notes-function 'org-ref-notes-function-many-files
     org-ref-completion-library 'org-ref-ivy-cite
     org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
     org-ref-default-bibliography (list "~/.emacs.d/ref.bib")
     org-ref-bibliography-notes "~/org/wiki/bibnotes.org"
     org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
     org-ref-notes-directory ethan/bibliography-notes
     org-ref-notes-function 'orb-edit-notes
     ))

(setq
 bibtex-completion-notes-path ethan/bibliography-notes
 bibtex-completion-bibliography "~/.emacs.d/ref.bib"
 bibtex-completion-pdf-field "file"
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
  ))

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
   org-noter-notes-search-path ethan/bibliography-notes
   )
  (require 'org-noter-pdftools))

;; (setq org-agenda-custom-commands
;;       `(("a" "Agenda"
;;          ((agenda "" ((org-agenda-span 2)))
;;           ;; Projects
;;           (tags "+PROJECT-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive-evilplans"
;;                 ((org-tags-exclude-from-inheritance '("PROJECT"))
;;                  (org-agenda-prefix-format "  ")
;;                  (org-agenda-overriding-header "Projects: ")
;;                  (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;           ;; Inbox
;;           (alltodo ""
;;                    ((org-agenda-prefix-format "%-6e ")
;;                     (org-agenda-files '("~/org/wiki/inbox.org"))
;;                     (org-agenda-overriding-header "Inbox: ")))
;;           (todo "WAITING-inactive"
;;                 ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;                  (org-agenda-prefix-format "%-6e ")
;;                  (org-agenda-overriding-header "Waiting: ")
;;                  (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;           ;; Unscheduled
;;           (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans"
;;                      ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;                       (org-agenda-prefix-format "%-6e ")
;;                       (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;                       (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;           ))
;;         ("e" "Emacs" (tags "emacs"))
;;         ("i" "Inbox" alltodo ""
;;          ((org-agenda-files '("~/org/wiki/inbox.org"))))
;;         ("t" tags-todo "-cooking"
;;          ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
;;         ("T" tags-todo "TODO=\"TODO\"-goal-routine-cooking-SCHEDULED={.+}" nil "~/cloud/agenda/nonroutine.html")
;;         ("f" tags-todo "focus-TODO=\"DONE\"-TODO=\"CANCELLED\"")
;;         ("x" "Column view" todo ""  ; Column view
;;          ((org-agenda-prefix-format "")
;;           (org-agenda-cmp-user-defined 'my/org-sort-agenda-items-todo)
;;           (org-agenda-view-columns-initially t)
;;           ))
;;         ;; Weekly review
;;         ("w" "Weekly review" agenda ""
;;          ((org-agenda-span 7)
;;           (org-agenda-log-mode 1)) "~/org/wiki/this-week.html")
;;         ("W" "Weekly review sans routines" agenda ""
;;          ((org-agenda-span 7)
;;           (org-agenda-log-mode 1)
;;           (org-agenda-tag-filter-preset '("-routine"))) "~/org/wiki/this-week-nonroutine.html")
;;         ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
;;         ("5" "Quick tasks" tags-todo "EFFORT>=\"0:05\"&EFFORT<=\"0:15\"")
;;         ("0" "Unestimated tasks" tags-todo "EFFORT=\"\"")
;;         ("gc" "Coding" tags-todo "@coding"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gw" "Writing" tags-todo "@writing"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gp" "Phone" tags-todo "@phone"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gd" "Drawing" tags-todo "@drawing"
;;          ((org-agenda-view-columns-initially t)))
;;         ("gh" "Home" tags-todo "@home"
;;          ((org-agenda-view-columns-initially t)))
;;         ("ge" "Errands" tags-todo "errands"
;;          ((org-agenda-view-columns-initially t))
;;          ("~/cloud/agenda/errands.html"))
;;         ("c" "Top 3 by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-sorting-strategy '(priority-up effort-down))
;;           (org-agenda-max-entries 3)))
;;         ("C" "All by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-sorting-strategy '(priority-down effort-down))
;;           (org-agenda-max-entries nil)))
;;         ("9" "Unscheduled top 3 by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-sorting-strategy '(priority-down effort-down))
;;           (org-agenda-max-entries 3)))
;;         ("(" "All unscheduled by context"
;;          ,my/org-agenda-contexts
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-sorting-strategy '(priority-down effort-down))
;;           ))
;;         ("d" "Timeline for today" ((agenda "" ))
;;          ((org-agenda-ndays 1)
;;           (org-agenda-show-log t)
;;           (org-agenda-log-mode-items '(clock closed))
;;           (org-agenda-clockreport-mode t)
;;           (org-agenda-entry-types '())))
;;         ("." "Waiting for" todo "WAITING")
;;         ("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine"
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-view-columns-initially nil)
;;           (org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;           (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
;;         ("r" "Unscheduled, untagged tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine-evilplans-computer-writing-phone-sewing-home-errands-shopping"
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-view-columns-initially nil)
;;           (org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;           (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
;;         ("s" "Someday" tags-todo "TODO=\"SOMEDAY\""
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-agenda-view-columns-initially nil)
;;           (org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-overriding-header "Someday: ")
;;           (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
;;         ("U" "Unscheduled tasks outside projects" tags-todo "-project-cooking-routine"
;;          ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
;;           (org-tags-exclude-from-inheritance nil)
;;           (org-agenda-view-columns-initially nil)
;;           (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
;;           (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
;;         ("P" "By priority"
;;          ((tags-todo "+PRIORITY=\"A\"")
;;           (tags-todo "+PRIORITY=\"B\"")
;;           (tags-todo "+PRIORITY=\"\"")
;;           (tags-todo "+PRIORITY=\"C\""))
;;          ((org-agenda-prefix-format "%-10c %-10T %e ")
;;           (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;         ("pp" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive"
;;          ((org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;         ("p." tags "+project-TODO=\"DONE\""
;;          ((org-tags-exclude-from-inheritance '("project"))
;;           (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;         ("S" tags-todo "TODO=\"STARTED\"")
;;         ("C" "Cooking"
;;          ((tags "vegetables")
;;           (tags "chicken")
;;           (tags "beef")
;;           (tags "pork")
;;           (tags "other"))
;;          ((org-agenda-files '("~/orgzly/cooking.org"))
;;           (org-agenda-view-columns-initially t)
;;           (org-agenda-sorting-strategy '(scheduled-up time-down todo-state-up)))
;;          )
;;         ("8" "List projects with tasks" my/org-agenda-projects-and-tasks
;;          "+PROJECT"
;;          ((org-agenda-max-entries 3)))))
