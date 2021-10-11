;; ;; this buffer is for text that is not saved, and for Lisp evaluation.
;; ;; To create a file, visit it with <open> and enter text in its buffer.

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
                         ))))
;;           (alltodo "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '(;; Each group has an implicit boolean OR operator between its selectors.
;;                          (:name "Work important"
;;                            :and (:priority>= "B" :category "Work" :todo ("TODO" "NEXT")))
;;                           (:name "Work other"
;;                            :and (:category "Work" :todo ("TODO" "NEXT")))
;;                           (:name "Important"
;;                            :priority "A")
;;                           (:priority<= "B"
;;                            ;; Show this section after "Today" and "Important", because
;;                            ;; their order is unspecified, defaulting to 0. Sections
;;                            ;; are displayed lowest-number-first.
;;                            :order 1)
;;                          (:name "Waiting"
;;                            :todo "WAITING"
;;                            :order 9)
;;                           (:name "On hold"
;;                            :todo "HOLD"
;;                            :order 10)))))))))
;; (
          ))))
(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
(setq foo "testing")
;; (truncate-string-to-width foo 4)
;; (truncate-string-to-width foo 4)
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; (setq org-agenda-custom-commands
;;       '(("z" "Hugo view"
;;           ((alltodo "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '(;; Each group has an implicit boolean OR operator between its selectors.
;;                           (:name "Today"
;;                            :deadline today
;;                            :transformer (--> it
;;                                              (truncate-string-to-width it 100))
;;                            :order 1
;;                            :face (:background "black"))
;;                           (:name "Passed deadline"
;;                            :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT" "INPROGRESS"))
;;                            :transformer (--> it
;;                                              (truncate-string-to-width it 80))
;;                            :order 2
;;                            :face (:background "#7f1b19"))
;;                           (:name "Habits"
;;                            :habit t
;;                            :order 3)
;;                           (:name "On hold"
;;                            :todo "HOLD"
;;                            :order 10)))))))))
;; (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

;; (setq org-super-agenda-groups
;;          '(
;;           (:name "Deadlines"
;;                   :deadline today
;;                   :order 0)
;;           (:name "Upcoming Deadlines"
;;                   :deadline future
;;                   :order 10)
;;           (:name "Habits"
;;                   :habit t
;;   :transformer (--> it
;;                     (truncate-string-to-width it 100))
;;                   :order 7)
;;            (:name "Routines"
;;                   :category "Routine"
;;                   :order 6)
;;            (:name "GTD Upkeep"
;;                   :category "GTD"
;;                   :order 8)
;;            (:name "Events"
;;                   :category "Event"
;;                   :category "Social"
;;                   :order 1)
;;            (:name "Emails and online tasks"
;;                   :tag "@email"
;;                   :order 1)
;;            (:name "Work tasks"
;;                   :time-grid t
;;                   :and (:tag "@work" :todo t :scheduled today)
;;                   :order 2)
;;            (:name "Deferred work tasks"
;;                   :and (:tag "@work" :not (:todo ("CNCL" "DONE")) :scheduled past)
;;                   :order 3)
;;            (:name "Information about today"
;;                   :category "Sunrise"
;;                   :category "Day info"
;;                   :order 9)
;;            (:name "Personal tasks"
;;                   :scheduled today
;;                   :order 4)
;;             (:name "Deferred personal tasks"
;;                   :and (:not (:todo ("CNCL" "DONE")) :scheduled past)
;;                   :order 5)))
