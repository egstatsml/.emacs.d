(use-package all-the-icons)

(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
