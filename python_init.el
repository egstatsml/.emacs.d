;;PYTHON

;; Enable elpy
(elpy-enable)
;;Indentation highlighting in Python Mode
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'company-mode)

;;creating function that will highlight the 72 column,
;;which can be used to ensure docstrings aren't exceeding this
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-d") 'my-docstring-column)))
;;turn the column marker on
(defun my-docstring-column ()
  (interactive)
  (column-marker-3 72)
  )

;; use IPython shell
(defun ipython ()
  (interactive)
      (term "/usr/bin/ipython"))

(when (require 'cython-mode nil 'no-error)
  (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)
;; use flycheck instead of flymake
;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; setting location for pylintrc
(setq-default flycheck-pylintrc "~/.emacs.d/.pylintrc")
;; To avoid having to mouse hover for the error message,
;; these functions make flymake error messages appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(require 'flymake)
(add-hook 'post-command-hook 'show-fly-err-at-point)
;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
