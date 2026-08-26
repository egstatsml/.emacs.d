;;; setup-org.el -*- lexical-binding: t; no-byte-compile: t; -*-

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

(provide 'setup-org)
