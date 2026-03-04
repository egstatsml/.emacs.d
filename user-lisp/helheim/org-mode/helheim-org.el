;;; helheim-org.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'hel-core)

;;; Keybindings

(with-eval-after-load 'org-keys
  ;; Normal state
  (hel-keymap-set org-mode-map :state 'normal
    "z '" 'org-edit-special
    "z ," 'org-insert-structure-template
    "z /" 'org-sparse-tree
    ;; "z n" 'org-narrow-to-subtree
    "g i" 'consult-org-heading
    "g x" 'org-open-at-point
    "g n" 'org-next-link
    "g N" 'org-previous-link
    "] l" 'org-next-link
    "[ l" 'org-previous-link)
  ;; <leader>
  (hel-keymap-set (keymap-lookup org-mode-map "C-c")
    "'"   nil ;; `org-edit-special' is moved to "z'"
    ","   nil ;; `org-priority' is moved to "z,"
    "/"   nil ;; `org-sparse-tree' is moved to "z/"
    "RET" 'dired-jump ;; rebind `org-ctrl-c-ret', which is also on "z RET"
    "a"   'org-attach
    "t i" 'org-toggle-inline-images
    "t l" 'org-toggle-link-display
    "t f" 'org-table-toggle-formula-debugger
    "t o" 'org-table-toggle-coordinate-overlays
    "i"   (cons "insert"
                (define-keymap
                  "l" '("insert link" . org-insert-link)
                  "m" 'yank-media
                  "d" 'org-deadline
                  "s" 'org-schedule
                  "t" 'org-time-stamp
                  "T" 'org-time-stamp-inactive
                  "Q" 'org-set-tags-command))
    "l"   (cons "links"
                (define-keymap
                  "l" 'org-insert-link
                  "i" 'org-insert-last-stored-link ;; "i" for insert
                  "s" 'org-store-link
                  "a" 'org-insert-all-links
                  "m" 'yank-media))))

(with-eval-after-load 'dired
  (hel-keymap-set dired-mode-map
    "C-c a" 'org-attach-dired-to-subtree))

;;; Config
(setq org-directory (expand-file-name "~/org/"))

(setopt org-insert-heading-respect-content nil
        org-M-RET-may-split-line '((default . t)
                                   (item . nil))
        org-return-follows-link t
        org-special-ctrl-a/e t
        org-pretty-entities t)

;; Open org links in the same window. Use "C-c &" to back to the link.
(with-eval-after-load 'ol
  (setf (alist-get 'file org-link-frame-setup) #'find-file))

(with-eval-after-load 'org-indent
  (blackout 'org-indent-mode))

(use-package hel-org :after org)

(use-package org-eldoc
  :ensure org-contrib
  :after org
  :custom (org-eldoc-breadcrumb-separator " → ")
  :config
  ;; Show target for link at point. Emacs has `help-at-pt-display-when-idle',
  ;; but its timer competes with Eldoc for the echo area, so for those who use
  ;; Eldoc in Emacs 31 `eldoc-help-at-pt' option was added.
  (if (version<= "31" emacs-version)
      (setopt eldoc-help-at-pt t) ;; since Emacs 31
    (define-advice org-eldoc-documentation-function (:before-until (&rest _) helheim)
      "Display link target in echo area when cursor/mouse is over it."
      (if-let ((url (thing-at-point 'url t)))
          (format "LINK: %s" url))))
  ;; HACK Fix #2972: infinite recursion when eldoc kicks in 'org' or 'python'
  ;;   src blocks.
  ;; TODO Should be reported upstream!
  (puthash "org" #'ignore org-eldoc-local-functions-cache)
  (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
  (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

;;;; Add padding to headings

(defcustom helheim-org-heading-padding 1.6
  "Add extra padding to Org mode headings to make them more spacious.
This is done by increasing the height of the space character between the stars
that denotes the heading level and the heading text. See help for
`set-face-attribute' -> `:height' for the meaning of the value.

If nil — extra padding will be disabled.

Must be set with `setopt' function!"
  :type 'number
  :group 'helheim
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (not value)
             (remove-hook 'org-font-lock-set-keywords-hook
                          'helheim-org--add-padding-to-headings)
           ;; else
           (-doto (make-face 'helheim-org-heading-padding-face)
             (set-face-attribute nil :height value))
           (add-hook 'org-font-lock-set-keywords-hook
                     'helheim-org--add-padding-to-headings))))

(defun helheim-org--add-padding-to-headings ()
  "Replace the 2nd element in `org-font-lock-extra-keywords' responsible for
heading fontification."
  (setf (nth 1 org-font-lock-extra-keywords)
        `(,(if org-fontify-whole-heading-line
               "^\\(\\**\\)\\(\\*\\)\\( \\)\\(.*\n?\\)"
             "^\\(\\**\\)\\(\\*\\)\\( \\)\\(.*\\)")
          (1 (helheimg-org--level-face 1))
          (2 (helheimg-org--level-face 2))
          (3 (helheimg-org--level-face 3))
          (4 (helheimg-org--level-face 4)))))

(defun helheimg-org--level-face (n)
  "Get the right face for match N in font-lock matching of headlines."
  (let* ((org-l0 (- (match-end 3) (match-beginning 1) 1))
         (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
         (face (if org-cycle-level-faces
                   (nth (% (1- org-l) org-n-level-faces) org-level-faces)
                 (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
    (cond ((eq n 1) (if org-hide-leading-stars 'org-hide face))
          ((eq n 2) face)
          ((eq n 3) 'helheim-org-heading-padding-face)
          (t (unless org-level-color-stars-only face)))))

;;;; Prettify symbols mode

;; You may delete the hooks you don't like with:
;;   (remove-hook 'org-mode-hook 'helheim-org-prettify-todo-keywords)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (add-hook 'org-mode-hook 'helheim-org-prettify-todo-keywords)
(add-hook 'org-mode-hook 'helheim-org-prettify-blocks)

(setq-default org-todo-keywords
              '((sequence "SOMEDAY" "TODO" "IN-PROGRESS" "WAIT" "|"
                          "DONE" "ARCHIVED" "CANCELLED")
                (sequence "READ" "IN-PROGRESS" "|" "DONE")))

(defun helheim-org-prettify-todo-keywords ()
  "Beautify org mode \"todo\" keywords using `prettify-symbols-mode'."
  (cl-callf append prettify-symbols-alist
    '(("SOMEDAY"     . ?󰒅) ; 󰔌
      ("TODO"        . ?󰄱) ; 󰝣
      ("IN-PROGRESS" . ?󰡖) ; 󱗝 󰜄 󰤌
      ("WAIT"        . ?)
      ("DONE"        . ?󰄵) ; 󰱒
      ("ARCHIVED"    . ?󱈎)
      ("CANCELLED"   . ?󰅘)
      ("READ"        . ?󰃃))))

(defun helheim-org-prettify-blocks ()
  "Beautify org mode block keywords using `prettify-symbols-mode'."
  (cl-callf append prettify-symbols-alist
    (eval-when-compile
      (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
              '(("#+begin_src"     . ?)
                ("#+end_src"       . ?)
                ("#+begin_example" . ?)
                ("#+end_example"   . ?)
                ("#+begin_quote"   . ?)
                ("#+end_quote"     . ?))))))

;;;; ID format

;; Use ISO 8601 timestamp.
(setopt org-id-method 'ts
        org-id-ts-format helheim-id-format
        org-id-link-to-org-use-id 'create-if-interactive)

;;;; org-attach

;; FIX: Link to attachment can't be oppend before `org-attach' is loaded,
;;   and `org-open-at-point' loads it only for headings, but not for links.
(cl-pushnew 'org-attach org-modules)

;; setting up faster access to GTD file
(defun ethan/open-gtd-file (arg)
  "Edit the gtd file.
ARG is file to open."
  (interactive "P")
  (ethan/open-file "~/org/gtd.org" arg))

(defun ethan/open-inbox-file (arg)
  "Edit the inbox file.
ARG is file to open."
  (interactive "P")
  (ethan/open-file "~/org/inbox.org" arg))

(defun ethan/open-someday-file (arg)
  "Edit the someday file.
ARG is file to open."
  (interactive "P")
  (ethan/open-file "~/org/someday.org" arg))

(defun ethan/open-calendar-file (arg)
  "Edit the calendar file.
ARG is file to open."
  (interactive "P")
  (ethan/open-file "~/org/calendar.org" arg))

(defun ethan/open-tickler-file (arg)
  "Edit the tickler file.
ARG is file to open."
  (interactive "P")
  (ethan/open-file "~/org/tickler.org" arg))


(defun my/org-todo ()
  "Set current state to TODO."
  (interactive)
  (org-todo "TODO"))

(defun my/org-inprogress ()
  "Set current state to INPROGRESS."
  (interactive)
  (org-todo "INPROGRESS")
  (org-clock-in))

(defun my/org-todo-done ()
  "Set current state to DONE."
  (interactive)
  (org-todo "DONE")
  (org-clock-out))

(defun my/org-waiting ()
  "Set current state to WAITING."
  (interactive)
  (org-todo "WAITING")
  (org-clock-out))

(defun my/org-next ()
  "Set current state to NEXT."
  (interactive)
  (org-todo "NEXT"))

(defun my/org-blocked ()
  "Set current state to BLOCKED."
  (interactive)
  (org-todo "BLOCKED"))


;; Archiving my Done Tasks
(defun my/org-archive-subtree-done-tasks ()
  "Archive done tasks in subtree.
Got from https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(defun my/org-archive-all-done ()
  "Iterate over all top-level headings in the current org buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (my/org-archive-subtree-done-tasks))
   ;; Match all headlines
   "LEVEL=1"
   ;; Scope: current buffer
   'file))

(use-package org
  :custom
  ;; org attach settings, mostly from helheim
  (org-attach-id-dir (expand-file-name "org-attach/" org-directory))
  (org-attach-method 'mv) ;; move
  (org-attach-store-link-p 'attached)
  (org-attach-preferred-new-method 'id)
  (org-attach-use-inheritance nil)
  (org-attach-dir-relative t)
  (org-attach-sync-delete-empty-dir t)
  (org-attach-id-to-path-function-list '(helheim-org-attach-id-ts-folder-format
                                         org-attach-id-uuid-folder-format
                                         identity))
  (org-todo-keywords
   '((sequence "TODO" "NEXT" "INPROGRESS" "BLOCKED" "|" "DONE")
     (sequnce "TOREAD" "|" "DONE")
     (sequence "INPROGRESS" "FEEDBACK" "WAITING" "VERIFY" "|" "DONE")))
  ;; Org visual and formatting stuffs
  ;; hide emphasise markers
  (org-hide-emphasis-markers t)
  ;; make sure code is highlighted properly
  (org-src-fontify-natively t)
  ;; number of spaces for indentation of code
  (org-edit-src-content-indentation 2)
  ;; want to show code at the start
  (org-hide-block-startup nil)
  ;; start with org documents to show just the content
  (org-startup-folded 'content)
  ;; number of empty lines needed to keep space between trees
  (org-cycle-separator-lines 2)
  ;; add timestamp when closing a task
  (org-log-done 'time)
  ;; I want my tags to be aligned properly
  ;;forcing image size if it is too large
  (org-image-actual-width '(600))
  :config
  (setopt org-attach-auto-tag "ATTACH")
  (cl-pushnew org-attach-auto-tag
              org-tags-exclude-from-inheritance
              :test #'equal)
  (keymap-set org-mode-map "<remap> <org-attach>" 'helheim-org-attach)
  ;; taken from https://www.reddit.com/r/emacs/comments/185e4k1/orgmode_tag_right_alignment/
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords 'org-mode
			  `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
			     (1 `(face nil
				       display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
			        prepend))) t)
  ;; capture
  (setq org-gtd-inbox-file (concat org-directory "inbox.org"))
  (setq org-capture-templates
	`(("i" "Inbox" entry  (file org-gtd-inbox-file)
	   ,(concat "* TODO %?\n"
		    "/Entered on/ %U"))
	  ("w"
	   "Default template"
	   entry
	   (file+headline "~/org/inbox.org" "Notes")
	   "* %^{Title}\n  Source: %:link %u\n %c\n  %i"
	   :empty-lines 1)))
  ;; ("w" "Web site" entry
  ;;  (file+olp "~/org/inbox.org" "Web")
  ;;  "* %a :website:\n\n%U %?\n\n%:initial")))
  ;; Agenda Files
  (setq org-agenda-files
	(list
	 "~/org/gtd.org"
	 "~/org/tickler.org"
	 "~/org/someday.org"
	 "~/org/calendar.org"
	 "~/org/inbox.org"))
  ;; Agenda Habits
  (setq
   org-habit-show-habits-only-for-today t
   org-habit-show-all-today t
   org-habit-preceding-days 7
   org-habit-following-days 2
   org-agenda-view-columns-initially nil
   ;; I want to show the habit column
   org-habit-graph-column 70
   org-agenda-hide-tags-regexp ".*HABIT.*")

  ;; faces for column mode
  ;; want to make sure that headings in column mode or the same font size as normal
  ;; text, otherwise alignment of the  columns gets all wonky
  (set-face-attribute 'org-column nil
		      :height (face-attribute 'default :height)
		      :family (face-attribute 'default :family))
  ;; Adding habit property
  (defun my-add-habit-property ()
    "Add EFFORT property to add to org-mode TODO task"
    (interactive)
    (org-set-property "STYLE" "habit")))

(defun helheim-org-attach-id-ts-folder-format (id)
  "Translate an ID based on a ISO8601 timestamp to a folder-path.
Place the attachment folder into year folder: 2025/20251104T161807"
  (if (< 4 (length id))
      (format "%s/%s"
              (substring id 0 4)
              id)))

(setq org-attach-commands
      '(((?a ?\C-a) org-attach-attach
         "Select a file and attach it to the task, using `org-attach-method'.")
        ((?c ?\C-c) org-attach-attach-cp
         "Attach a file using copy method.")
        ((?m ?\C-m) org-attach-attach-mv
         "Attach a file using move method.")
        ((?l ?\C-l) org-attach-attach-ln
         "Attach a file using link method.")
        ((?y ?\C-y) org-attach-attach-lns
         "Attach a file using symbolic-link method.")
        ((?u ?\C-u) org-attach-url
         "Attach a file from URL (downloading it).")
        ((?b) org-attach-buffer
         "Select a buffer and attach its contents to the task.")
        ((?n ?\C-n) org-attach-new
         "Create a new attachment, as an Emacs buffer.")
        ((?z ?\C-z) org-attach-sync
         "Synchronize the current node with its attachment\n directory, in case \
you added attachments yourself.\n")
        ((?o ?\C-o) org-attach-open
         "Open current node's attachments.")
        ((?O) org-attach-open-in-emacs
         "Like \"o\", but force opening in Emacs.")
        ((?f ?\C-f) org-attach-reveal-in-emacs
         "Open current node's attachment directory in Dired.  Create if missing.")
        ((?F) org-attach-reveal
         "Like \"f\", but try to open in system file manager.\n")
        ((?d ?\C-d) org-attach-delete-one
         "Delete one attachment, you will be prompted for a file name.")
        ((?D) org-attach-delete-all
         "Delete all of a node's attachments.  A safer way is\n to open the \
directory in dired and delete from there.\n")
        ((?s ?\C-s) org-attach-set-directory
         "Set a specific attachment directory for this entry. Sets DIR property.")
        ((?S ?\C-S) org-attach-unset-directory
         "Unset the attachment directory for this entry.  Removes DIR property.")
        ((?q) (lambda () (interactive) (message "Abort")) "Abort.")))

(defun helheim-org-attach ()
  "The dispatcher for attachment commands.
Like `org-attach' but tuned for Emacs Helheim."
  (interactive)
  (let (marker)
    (when (eq major-mode 'org-agenda-mode)
      (setq marker (or (get-text-property (point) 'org-hd-marker)
                       (get-text-property (point) 'org-marker)))
      (unless marker
        (error "No item in current line")))
    (org-with-point-at marker
      (let ((dir (org-attach-dir nil :no-fs-check)))
        (cl-assert dir nil "Cannot derive attachment directory from ID")
        (deactivate-mark)
        (if (not (featurep 'org-inlinetask))
            (org-back-to-heading-or-point-min t)
          ;; else
          (if (org-inlinetask-in-task-p)
              (org-inlinetask-goto-beginning)
            ;; else
            (org-with-limited-levels
             (org-back-to-heading-or-point-min t))))
        (let (key)
          (save-excursion
            (save-window-excursion
              (unless org-attach-expert
                (helheimg--org-attach-buffer dir)
                (org-fit-window-to-buffer (get-buffer-window "*Org Attach*")))
              (unwind-protect
                  (progn
                    (message "Select command: [%s]"
                             (concat (mapcar #'caar org-attach-commands)))
                    (while (and (setq key (read-char-exclusive))
                                (memq key '(?\C-f ?\C-b)))
	              (org-scroll (alist-get key '((?\C-f . ?\C-n)
                                                   (?\C-b . ?\C-p))))))
                (-some->> (get-buffer-window "*Org Attach*" t)
                  (quit-window :kill))
	        (-some-> (get-buffer "*Org Attach*")
                  (kill-buffer)))))
          (if-let* ((command (-some (lambda (entry)
				      (and (memq key (nth 0 entry))
                                           (nth 1 entry)))
			            org-attach-commands))
                    ((commandp command)))
	      (command-execute command)
	    (error "No such attachment command: %c" key)))))))

(defun helheimg--org-attach-buffer (dir)
  (switch-to-buffer-other-window "*Org Attach*")
  (erase-buffer)
  (setq cursor-type nil)
  (setq header-line-format (format "Use %s and %s for scrolling"
                                   (propertize "C-f" 'face 'help-key-binding)
                                   (propertize "C-b" 'face 'help-key-binding)))
  (insert
   (concat
    "Attachment folder:\n\n"
    (propertize (abbreviate-file-name dir)
                'face 'font-lock-string-face)
    "\n\n  "
    (if (file-directory-p dir)
        (propertize "Exist" 'face 'success)
      (propertize "Does not exist" 'face 'warning))
    "\n\n"
    "Select an Attachment Command:\n\n"
    (mapconcat (lambda (entry)
	         (pcase entry
		   (`((,key . ,_) ,_ ,docstring)
		    (format "%s       %s"
                            (-> (char-to-string key)
                                (propertize 'face 'help-key-binding))
                            (replace-regexp-in-string "\n\\([\t ]*\\)"
                                                      "        "
                                                      docstring nil nil 1)))
		   (_
		    (user-error "Invalid `org-attach-commands' item: %S"
			        entry))))
	       org-attach-commands
	       "\n")))
  (goto-char (point-min)))

;;;; images

(setopt org-startup-with-inline-images t
        org-cycle-inline-images-display t
        org-image-actual-width '(300))

;;;; org-cliplink

(use-package org-cliplink
  :ensure t
  :defer t
  :custom
  (org-cliplink-max-length nil)
  (org-cliplink-ellipsis "…"))

(with-eval-after-load 'org-keys
  (hel-keymap-set org-mode-map
    "<remap> <org-insert-link>" 'helheim-org-insert-link))

;; Based on https://xenodium.com/emacs-dwim-do-what-i-mean/
(defun helheim-org-insert-link ()
  "Like `org-insert-link' but with some \"do what i mean\" behavior.
- If URL is in clipboard — use it.
- If selection is active — use it as link description.
- Automatically fetch URL title from its HTML tag.
- Fallback to `org-insert-link'."
  (interactive)
  (let ((point-at-link (org-in-regexp org-link-any-re 1))
        (clipboard-url (if (string-match-p "^http" (current-kill 0))
                           (current-kill 0)))
        (region-content (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning)
                                                            (region-end)))))
    (cond ((and region-content clipboard-url (not point-at-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-at-link))
           (org-cliplink))
          (t
           (call-interactively 'org-insert-link)))))


;;  Making org pretty
(use-package org-modern
  :ensure (org-modern
	   :host github
	   :repo "minad/org-modern")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :init
  (setq org-modern-hide-stars 'leading
        org-modern-table nil
        org-modern-tag t
        org-modern-list
        '(;; (?- . "-")
	  (?* . "•")
	  (?+ . "‣")))
  :config
  ;; org modern has a tough time with tables in variable-pitch mode when there
  ;; are times in the cells. For org-clock-reports, this is pretty common, so I am
  ;; going to just disable org modern for tables and ensured am using fixed pitch
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(use-package org-modern-indent
  :ensure (org-modern-indent
	   :host github
	   :repo "jdtsmith/org-modern-indent")
  :after org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-latex-preview
  :defer t
  ;; :after org
  :config
  ;; adding amsmath to the preamble, is needed when using pdflatex
  (setq org-latex-preview-preamble
	"\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usepackage{amsmath}")
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
	     :page-width 0.8)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-mode)

  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
	'(next-line previous-line mwheel-scroll
		    scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)
  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)
  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25)
  ;; consistently centered previews
  (defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
		  ((or (eq (org-element-type elem) 'latex-environment)
		       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
		  (img (overlay-get ov 'display))
		  (prop `(space :align-to (- center (0.55 . ,img))))
		  (justify (propertize " " 'display prop 'face 'default)))
	(overlay-put ov 'justify justify)
	(overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
	(progn
	  (add-hook 'org-latex-preview-overlay-open-functions
		    #'my/org-latex-preview-uncenter nil :local)
	  (add-hook 'org-latex-preview-overlay-close-functions
		    #'my/org-latex-preview-recenter nil :local)
	  (add-hook 'org-latex-preview-overlay-update-functions
		    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
		   #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
		   #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
		   #'my/org-latex-preview-uncenter))))
;;; .
(provide 'helheim-org)
;;; helheim-org.el ends here
