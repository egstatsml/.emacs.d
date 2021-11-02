;; for agenda commands
;;
;; set agenda files
(setq org-agenda-files
      (list "~/org/wiki/gtd.org"
            "~/org/wiki/tickler.org"
            "~/org/wiki/someday.org"
            "~/org/wiki/inbox.org"))
;; make sure tags don't over run multiple lines in agenda view
(setq org-agenda-align-tags-to-column -100)
;; I want to show the habit column
(setq org-habit-graph-column 60)

;; now loading config for super agenda
(setq org-agenda-custom-commands
      '(("z" "Hugo view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :deadline today
                          :order 1)
                         (:name "Habit"
                          :date today
                          :habit t
                          :order 3)
                         (:name "Passed deadline"
                           :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT" "INPROGRESS"))
                           :transformer (--> it
                                             (truncate-string-to-width it 100))
                           :face (:background "#41363C"))
                         (:name "DEADLINES"
                          :tag "DEADLINE"
                          :transformer (--> it
                                             (truncate-string-to-width it 100))
                           :face (:background "#41363C"))
                         ))))))
        ("q" "Quick Tasks"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Super Quick"
                          :discard (:habit t)
                          :effort< "0:30"
                          :order 1)
                         (:name "Less Quick"
                          :discard (:habit t)
                          :effort< "1:00"
                          :order 2)
                         (:name "Habit"
                          :date today
                          :habit t
                          :discard (:not (:habit t))
                          :order 3)
                        ))))))
        ("h" "Habits"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Habit"
                          :discard (:not (:habit t))
                          :date today
                          :habit t
                          :order 1)
                        ))))))

          ))
(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

;; Some additional helper functions that I have found from others

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

;; ;;(bind-key "<apps> a" 'org-agenda)

;; (defun my/org-agenda-project-agenda ()
;;   "Return the project headline and up to `org-agenda-max-entries' tasks."
;;   (save-excursion
;;     (let* ((marker (org-agenda-new-marker))
;;            (heading
;;             (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
;;            (org-agenda-restrict t)
;;            (org-agenda-restrict-begin (point))
;;            (org-agenda-restrict-end (org-end-of-subtree 'invisible))
;;            ;; Find the TODO items in this subtree
;;            (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
;;       (org-add-props heading
;;           (list 'face 'defaults
;;                 'done-face 'org-agenda-done
;;                 'undone-face 'default
;;                 'mouse-face 'highlight
;;                 'org-not-done-regexp org-not-done-regexp
;;                 'org-todo-regexp org-todo-regexp
;;                 'org-complex-heading-regexp org-complex-heading-regexp
;;                 'help-echo
;;                 (format "mouse-2 or RET jump to org file %s"
;;                         (abbreviate-file-name
;;                          (or (buffer-file-name (buffer-base-buffer))
;;                              (buffer-name (buffer-base-buffer))))))
;;         'org-marker marker
;;         'org-hd-marker marker
;;         'org-category (org-get-category)
;;         'type "tagsmatch")
;;       (concat heading "\n"
;;               (org-agenda-finalize-entries list)))))

;; (defun my/org-agenda-projects-and-tasks (match)
;;   "Show TODOs for all `org-agenda-files' headlines matching MATCH."
;;   (interactive "MString: ")
;;   (let ((todo-only nil))
;;     (if org-agenda-overriding-arguments
;;         (setq todo-only (car org-agenda-overriding-arguments)
;;               match (nth 1 org-agenda-overriding-arguments)))
;;     (let* ((org-tags-match-list-sublevels
;;             org-tags-match-list-sublevels)
;;            (completion-ignore-case t)
;;            rtn rtnall files file pos matcher
;;            buffer)
;;       (when (and (stringp match) (not (string-match "\\S-" match)))
;;         (setq match nil))
;;       (when match
;;         (setq matcher (org-make-tags-matcher match)
;;               match (car matcher) matcher (cdr matcher)))
;;       (catch 'exit
;;         (if org-agenda-sticky
;;             (setq org-agenda-buffer-name
;;                   (if (stringp match)
;;                       (format "*Org Agenda(%s:%s)*"
;;                               (or org-keys (or (and todo-only "M") "m")) match)
;;                     (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
;;         (org-agenda-prepare (concat "TAGS " match))
;;         (org-compile-prefix-format 'tags)
;;         (org-set-sorting-strategy 'tags)
;;         (setq org-agenda-query-string match)
;;         (setq org-agenda-redo-command
;;               (list 'org-tags-view `(quote ,todo-only)
;;                     (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
;;         (setq files (org-agenda-files nil 'ifmode)
;;               rtnall nil)
;;         (while (setq file (pop files))
;;           (catch 'nextfile
;;             (org-check-agenda-file file)
;;             (setq buffer (if (file-exists-p file)
;;                              (org-get-agenda-file-buffer file)
;;                            (error "No such file %s" file)))
;;             (if (not buffer)
;;                 ;; If file does not exist, error message to agenda
;;                 (setq rtn (list
;;                            (format "ORG-AGENDA-ERROR: No such org-file %s" file))
;;                       rtnall (append rtnall rtn))
;;               (with-current-buffer buffer
;;                 (unless (derived-mode-p 'org-mode)
;;                   (error "Agenda file %s is not in `org-mode'" file))
;;                 (save-excursion
;;                   (save-restriction
;;                     (if org-agenda-restrict
;;                         (narrow-to-region org-agenda-restrict-begin
;;                                           org-agenda-restrict-end)
;;                       (widen))
;;                     (setq rtn (org-scan-tags 'my/org-agenda-project-agenda matcher todo-only))
;;                     (setq rtnall (append rtnall rtn))))))))
;;         (if org-agenda-overriding-header
;;             (insert (org-add-props (copy-sequence org-agenda-overriding-header)
;;                         nil 'face 'org-agenda-structure) "\n")
;;           (insert "Headlines with TAGS match: ")
;;           (add-text-properties (point-min) (1- (point))
;;                                (list 'face 'org-agenda-structure
;;                                      'short-heading
;;                                      (concat "Match: " match)))
;;           (setq pos (point))
;;           (insert match "\n")
;;           (add-text-properties pos (1- (point)) (list 'face 'org-warning))
;;           (setq pos (point))
;;           (unless org-agenda-multi
;;             (insert "Press `C-u r' to search again with new search string\n"))
;;           (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
;;         (org-agenda-mark-header-line (point-min))
;;         (when rtnall
;;           (insert (mapconcat 'identity rtnall "\n") ""))
;;         (goto-char (point-min))
;;         (or org-agenda-multi (org-agenda-fit-window-to-buffer))
;;         (add-text-properties (point-min) (point-max)
;;                              `(org-agenda-type tags
;;                                                org-last-args (,todo-only ,match)
;;                                                org-redo-cmd ,org-agenda-redo-command
;;                                                org-series-cmd ,org-cmd))
;;         (org-agenda-finalize)
;;         (setq buffer-read-only t)))))


;; (defvar my/org-agenda-contexts
;;   '((tags-todo "phone")
;;     (tags-todo "work")
;;     (tags-todo "drawing")
;;     (tags-todo "coding")
;;     (tags-todo "writing")
;;     (tags-todo "computer")
;;     (tags-todo "home")
;;     (tags-todo "errands"))
;;   "Usual list of contexts.")
;; (defun my/org-agenda-skip-scheduled ()
;;   (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
