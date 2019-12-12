;;init.el
;;
;;Author: Ethan Goan (With a huge amount of help from the ELisp community)
;;
;;Description:
;;My configuration file for Emacs, where I have focused on configuring it for
;;Python, LaTeX, org-mode and Matlab
;;
;;
;;

;;(require 'cask "~/.cask/cask.el")
;;(cask-initialize)
;; Setting up directories that have additional plugins
;;this will look recursively throughout packages directory
(let ((default-directory  "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))
;; ensuring mu4e is in the path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;setting package archives
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("org" . 20)
        ("melpa" . 30)
        ("gnu" . 10)))

;;setting up faster access to init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; delete whitspace upon saving a file
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; using Forge with Magit
(with-eval-after-load 'magit
  (require 'forge))



;; Some global settings

;;highlighting parenthesis etc.
(show-paren-mode 1)
(global-linum-mode 1)
;;company mode
;;(add-hook 'after-init-hook 'global-company-mode)
;;enable pretty control mode
(require 'pp-c-l)           ; Load this library
(pretty-control-l-mode 1)
(setq pp^L-^L-string-function (lambda (win)
				(make-string fill-column ?-)))
(setq-default indent-tabs-mode nil)
(require 'package)
(package-initialize)  ;load and activate packages
;; enable wakatime
(setq wakatime-api-key "f53a8041-948d-4751-aeca-2ac27260e8f2")
(setq wakatime-cli-path "/home/ethan/.local/bin/wakatime")
(global-wakatime-mode)
;;

;;enable column-enforce mode for sorce code modes
(require 'column-enforce-mode)
(package-initialize)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(require 'column-marker)

;;spell checking

(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-<f8>") 'flyspell-buffer)
(global-set-key (kbd "<f10>") 'flyspell-buffer)
(global-set-key (kbd "<f7>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "<f9>") 'flyspell-check-next-highlighted-word)
(setq ispell-dictionary "british")    ;set the default dictionary
;;Go to next mispelt word
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))


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

;;PYTHON
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

(add-hook 'post-command-hook 'show-fly-err-at-point)
;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))



;;settings for R
(require 'ess-view)
;;view dataframe with C-x w

;;change assign shortkey to semi-colon
(add-hook  'R-mode-hook 'r-smart-mode)
(defun r-smart-mode ()
  (local-set-key (kbd ";")  (lambda () (interactive) (insert " <- ")))
  (ess-toggle-underscore nil))

(setq ess-default-style 'RStudio)
;; For STAN
(require 'stan-mode)


;;settings for MATLAB
;;(add-to-list 'load-path "~/.emacs.d/packages/matlab/matlab.el")
;;(load-library "matlab-load")
;;(add-hook 'matlab-mode-hook 'company-mode)
;;(add-hook 'matlab-shell-mode-hook 'company-mode)
;;(setq auto-mode-alist
;;    (cons
;;     '("\\.m$" . matlab-mode)
;;     auto-mode-alist))


;;LaTeX

;;defaulting to biblatex dialect
(setq-default bibtex-dialect 'biblatex)
(setq TeX-parse-self t)
;;make it so we have to specify the main file whenever creating a TeX file
(setq-default TeX-master nil)
;; set to pdfmode so that is uses pdflatex by default
(setq TeX-PDF-mode t)
;;enable flyspell and run it on all LaTeX buffers
(add-hook  'LaTeX-mode-hook 'flyspell-mode)
(add-hook  'LaTeX-mode-hook 'flyspell-buffer)
;;setting up company mode for auxtex
(require 'company-auctex)
(company-auctex-init)
(add-hook  'LaTeX-mode-hook 'company-mode)
;; adding RefTeX support
(require `reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(add-to-list 'company-backends
              '(company-reftex-labels))
;; adding flymake to check my syntax on the fly
(require 'flymake)
(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
;; using Latexmk as the default latex command to compile
(require 'auctex-latexmk)
(auctex-latexmk-setup)
;;(setq auctex-latexmk-inherit-TeX-PDF-mode t)
;;(setq TeX-command-default "LatexMk")
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; setting the viewing program
;; setting to qpdfview on Manjaro
(with-eval-after-load "tex"
  (add-to-list 'TeX-view-program-list '("evince" "/usr/bin/evince %o"))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("evince")))
;;for langtool
(setq langtool-language-tool-jar "~/.emacs.d/packages/LanguageTool-4.1/languagetool-commandline.jar")
(require 'langtool)

;;config after we load a LaTeX doc.
(eval-after-load "latex"
  '(progn
     (add-hook 'LaTeX-mode-hook
               (lambda ()
                 ;;setting up keys for ebib
                 (local-set-key (kbd "C-c e") 'ebib)
                 (local-set-key (kbd "C-c i") 'ebib-insert-citation)
                 (local-set-key (kbd "C-c o") 'ebib-load-bibtex-file)
                 ;;setting up keys for texcount
                 (local-set-key (kbd "C-c c") 'latex-word-count)
                 (local-set-key (kbd "C-c w") 'latex-word-count-master)
                 ;;setting up keys for writegood mode
                 (local-set-key (kbd "C-c g") 'writegood-grade-level)
                 (local-set-key (kbd "C-c r") 'writegood-reading-ease)))))

;; nomenclature for latex
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                  (lambda (name command file)
                    (TeX-run-compile name command file)
                    (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                  nil t :help "Create nomenclature file")))

;;count words in single file
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "~/.emacs.d/packages/texcount/texcount.pl "
                         ; "uncomment then options go here "
                         (buffer-file-name))))
;;count words in entire document
(defun latex-word-count-master ()
  (interactive)
  (if (eq TeX-master t)
      (setq master (buffer-file-name))
    (setq master (concat (expand-file-name TeX-master) ".tex")))
  (shell-command (concat "~/.emacs.d/packages/texcount/texcount.pl "
                         "-dir "
                         "-unicode "
                         "-inc "
                         master)))


;;Some of my other custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t)
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
 '(org-agenda-files (quote ("~/org/wiki/TODO.org")))
 '(org-startup-folded t)
 '(org-startup-truncated nil)
 '(org-wiki-template
   "#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  content
#+OPTIONS: \\n:t
#+OPTIONS: H:3 num:1
#+SETUPFILE: /home/ethan/org/wiki/org-html-themes-master/setup/theme-readtheorg-local.setup

- [[wiki:index][Index]]

- Related:

* %n
")
 '(package-selected-packages
   (quote
    (cmake-mode wakatime-mode matlab-mode htmlize ghub mu4e-alert mu4e-conversation mu4e-jump-to-list mu4e-maildirs-extension mu4e-query-fragments ebib xref-js2 writegood-mode stan-mode org-wiki markdown-mode magit langtool helm-bibtex excorporate ess-view ess-smart-underscore ess-smart-equals ess-R-data-view auto-complete-auctex ac-html)))
 '(python-indent-offset 2)
 '(wakatime-python-bin nil))

;;Custom Keybindings
(global-set-key (kbd "C-x g") 'magit-status)

;;Insert four spaces
(defun my-insert-four ()
  (interactive)
  (insert "    "))
(global-set-key (kbd "C-x <up>") 'my-insert-four)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
