;;org-wiki mode
;; add timestamp when closing a task
(setq org-log-done 'time)
;; initialising org-wiki mode
(package-initialize)
(require 'org-wiki)
(package-initialize)
(require 'helm-config)
;;turn line numbers off
(defun nolinum ()
  (linum-mode 0))
(add-hook 'org-mode-hook 'nolinum)
;;
(setq org-wiki-location-list
      '(
        "~/org/wiki"    ;; First wiki (root directory) is the default.
        "~/org/blog"
        ))
;;forcing image size if it is too large
(setq org-image-actual-width '(600))
;; Initialize first org-wiki-directory or default org-wiki
(setq org-wiki-location (car org-wiki-location-list))

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

;;function for inserting the bibtex text
(defun my-insert-bibtex ()
  (interactive)
  (insert "#+BEGIN_SRC bibtex \n\n#+END_SRC")
  (previous-line)
    (insert "    ")
    )
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

;;add some more TODO list keywords
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
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
(defun my-insert-template ()
  (interactive)
  (insert "** Title of Article
*** Info
Author:
Journal:
Year:
Link:
Bibtex:
Code:

**** Pros
**** Cons
*** Abstract:
*** Notes:
**** Intro
**** Related Work
**** Results")
  (previous-line)
  (insert "    ")
  )

;;setting up my local key-bindings
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c a") 'org-agenda)))
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
