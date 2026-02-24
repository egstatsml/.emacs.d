;; agenda setup
;; I will do most of my agenda config with org-ql and org-super-agenda
(use-package org-agenda
  :if my/graphical
  :after org
  :commands org-agenda
  :bind (:map org-agenda-mode-map
	      ("C-c C-t" . nil)
	      ("C-c C-t C-t" . 'org-agenda-todo)
	      ("C-c C-t C-d" . 'my/org-agenda-done)))

;; Org-super-agenda
(use-package org-super-agenda
  :if my/graphical
  :ensure t
  :if my/graphical
  :after org
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  ;; making evil bindings work with fancy agenda
  (setq org-super-agenda-header-map (make-sparse-keymap))
  ;; ;; now loading config for super agenda
  (setq org-agenda-span 100)

  (setq org-agenda-custom-commands
	'(("z" "Hugo view"
	   ((agenda "" ((org-agenda-span 100)
			(org-agenda-start-day nil) ;; needed for doom https://github.com/alphapapa/org-super-agenda/issues/189#issuecomment-768498697
			(org-super-agenda-groups
			 '(
			   (:name "Habit"
				  :habit t
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 8)
			   (:name "Super Quick"
				  :and (:effort< "0:30" :not (:todo "DONE") :not (:tag "CALENDAR"))
				  :effort< "0:30"
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 2)
			   (:name "Less Quick"
				  :and (:effort< "1:00" :not (:todo "DONE"))
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 3)
			   (:name "CALENDAR"
				  :order 1
				  :tag "CALENDAR"
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :face (font-lock-builtin-face))
			   (:name "Today"
				  ;; :time-grid t
				  :date today
				  :todo "TODAY"
				  :deadline today
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 4)
			   (:name "Passed deadline"
				  :order 6
				  :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT" "INPROGRESS"))
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :face (font-lock-keyword-face))
			   (:name "DEADLINES"
				  :order 7
				  :tag "DEADLINE"
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :face (:background "#41363C"))
			   (:name "PhD"
				  :tag "PHD"
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 5)
			   ))))))
	  ("p" "Priority view"
	   ((agenda "" ((org-agenda-span 7)
			(org-agenda-start-day nil) ;; needed for doom https://github.com/alphapapa/org-super-agenda/issues/189#issuecomment-768498697
			(org-super-agenda-groups
			 '(
			   (:name "Habit"
				  :habit t
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 10)
			   (:name "Super Quick"
				  :and (:effort< "0:30" :not (:todo "DONE"))
				  :effort< "0:30"
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 2)
			   (:name "Less Quick"
				  :and (:effort< "1:00" :not (:todo "DONE"))
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 3)
			   (:name "CALENDAR"
				  :order 1
				  :tag "CALENDAR"
				  :transformer (--> it
						    (truncate-string-to-width it 80)))
			   (:name "Today"
				  ;; :time-grid t
				  :date today
				  :todo "TODAY"
				  :deadline today
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :order 5)
			   (:name "Passed deadline"
				  :order 4
				  :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT" "INPROGRESS"))
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :face (font-lock-keyword-face))
			   (:name "DEADLINES"
				  :order 9
				  :tag "DEADLINE"
				  :transformer (--> it
						    (truncate-string-to-width it 80))
				  :face (:background "#41363C"))
			   (:name "Top Priority"
				  :priority>="A"
				  :order 6)
			   (:name "Moderate Priority"
				  :priority "B"
				  :order 7)
			   (:name "Low Priority"
				  :priority<= "C"
				  :order 8)
			   (:discard (:tag "WEEKLY"))
			   )
			 )))))
	  ("q" "Quick Tasks"
	   ((agenda "" ((org-agenda-span 'day)
			(org-agenda-start-day nil) ;; needed for doom https://github.com/alphapapa/org-super-agenda/issues/189#issuecomment-768498697
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
				  :habit    t
				  :discard (:not (:habit t))
				  :order 3)
			   ))))))
	  ("h" "Habits"
	   ((agenda "" ((org-agenda-span 'day)
			(org-agenda-start-day nil) ;; needed for doom https://github.com/alphapapa/org-super-agenda/issues/189#issuecomment-768498697
			(org-super-agenda-groups
			 '((:name "Habit"
				  :tag "HABIT"
				  :discard (:not (:tag "HABIT"))
				  :date today
				  :habit t
				  :order 1)
			   ))))))
	  ("G" "Today"
	   ((agenda "" ((org-agenda-span 'day)
			(org-agenda-start-day nil) ;; needed for doom https://github.com/alphapapa/org-super-agenda/issues/189#issuecomment-768498697
			(org-super-agenda-groups
			 '(

			   (:name "Habit"
				  :discard (:not (:scheduled t))
				  :discard (:tag "WEEKLY")
				  :tag "HABIT"
				  :order 2)
			   (:name "Today"
				  :discard (:not (:scheduled t))
				  :discard (:tag "WEEKLY")
				  :time-grid t
				  ;; :not (:tag "HABIT")
				  ;; :not (:habit t)
				  :scheduled t
				  :date today
				  :order 1
				  )
			   ))))))
	  ("d" "Deadlines"
	   ((org-ql-block '(and (todo "TODO")
				(tags "DEADLINE"))
			  ((org-ql-block-header "Deadlines")))))
	  )))

;;; Org-ql
;; making sure org-ql is loaded
(use-package org-ql
  :ensure t
  :defer t
  :if my/graphical
  :bind (("M-o a" . 'ethan/open-agenda)
	 :map org-ql-view-map
	 ("C-c d" . 'my/org-agenda-done))
  :init
  ;; open my agenda and habits for a vertical screen
  (defun ethan/open-agenda ()
    (interactive)
    (org-ql-view "Agenda")
    (delete-other-windows)
    (hide-mode-line-mode 1)
    (split-window-below)
    (other-window 1)
    (org-agenda nil "G")
    (hide-mode-line-mode 1)
    (window-resize nil (- 20 (window-size))))
  :config
  (require 'org-ql-view)
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (defun my/agenda-header-string (it)
    ;; (interactive)
    (let* ((st (split-string it ":"))
	   (header (car st))
	   (tags (cdr st))
	   (tag-string (concat ":" (string-join tags ":")))
	   (header-truncated (truncate-string-to-width header (- 80 (length tags))))
	   (agenda-title (concat header-truncated " " tag-string)))
      (concat header-truncated " " tag-string)))

  ;; (message "%s" agenda-title)))


  ;; add my agenda to the org-ql-views
  (add-to-list
   'org-ql-views
   (cons
    "Agenda"
    (list
     :buffers-files '("~/org/gtd.org" "~/org/calendar.org")
     :query '(and (todo) (not (habit)))
     ;; (and (not (done))
     ;;      (not(habit))
     ;;      (deadline auto))
     :sort '(todo priority date)
     :super-groups
     '((:name "DEADLINES"
	      :order 0
	      :tag "DEADLINE"
	      ;; :transformer (--> it (truncate-string-to-width it 60)))
	      :transformer (--> it (my/agenda-header-string it)))
       (:name
	"Overdue"
	:order 1
	:and
	(:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT" "INPROGRESS"))
	:transformer (--> it (truncate-string-to-width it 80))
	:face (font-lock-keyword-face))
       (:name
	"Today"
	;; :time-grid t
	;; :date today
	:and (:todo ("TODO" "WAITING" "HOLD" "NEXT" "INPROGRESS") :deadline today)
	:transformer (--> it (my/agenda-header-string it))
	:order 2)
       (:name "In Progress" :todo "INPROGRESS" :order 3)
       (:name "Next" :todo "NEXT" :order 4)
       (:name "Waiting" :todo "WAITING" :order 5)
       (:name "Blocked" :todo "BLOCKED" :order 6)
       (:name "SUPER Quick" :tag "SUPERQUICK" :order 7)
       (:name
	"Quick"
	:tag "QUICK"
	:transformer (--> it (my/agenda-header-string it))
	:order 8)
       (:name "High Priority" :priority "A" :order 9)
       (:name "Moderate Priority" :priority "B" :order 10)
       (:name "Low Priority" :priority "C" :order 11)
       (:name "WEEKLY" :tag "WEEKLY" :order 12)
       (:name
	"CALENDAR"
	:order 13
	:tag "CALENDAR"
	:transformer (--> it (my/agenda-header-string it))
	:face (font-lock-builtin-face))
       (:name "REMINDER"
	      :order 14
	      :tag "REMINDER"
	      ;; :transformer (--> it (truncate-string-to-width it 60)))
	      :transformer (--> it (my/agenda-header-string it))))
     :title "Agenda"))))

(provide 'helheim-agenda)
