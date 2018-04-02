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

;; Setting up directories that have additional plugins
;;this will look recursively throughout packages directory
(let ((default-directory  "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))
;;setting package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))


;; Some global settings

;;highlighting parenthesis etc.
(show-paren-mode 1)
(global-linum-mode 1)
;;enable pretty control mode
(require 'pp-c-l)           ; Load this library
(pretty-control-l-mode 1)
(setq pp^L-^L-string-function (lambda (win)
				(make-string fill-column ?-)))
(setq-default indent-tabs-mode nil)
(require 'package)
(package-initialize)  ;load and activate packages, including auto-complete
(ac-config-default)
(global-auto-complete-mode t)
;;enable column-enforce mode for sorce code modes
(require 'column-enforce-mode)
(package-initialize)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(require 'column-marker)


;;org-wiki mode
(package-initialize)
(require 'org-wiki)
(package-initialize)
(require 'helm-config)
;;
(setq org-wiki-location-list
      '(
        "~/org/wiki"    ;; First wiki (root directory) is the default. 
        ))
;;forcing image size if it is too large
(setq org-image-actual-width '(600))

;; Initialize first org-wiki-directory or default org-wiki 
(setq org-wiki-location (car org-wiki-location-list))
;;now setting up some alias commands for org-wiki mode
(defalias 'w-i #'org-wiki-index)
(defalias 'w-in #'org-wiki-insert)
(defalias 'w-l #'org-wiki-latex)
(defalias 'w-lin #'org-wiki-insert-latex)
(defalias 'w-e #'org-wiki-export-html) ;;exports the entire (well most) of the wiki
(defalias 'w-h #'org-html-export-to-html);;exports a single page
(defalias 'w-im #'org-display-inline-images)
(defalias 'w-b #'org-wiki-insert-new);;use for inserting bibtex entries
(global-set-key (kbd "C-c l") 'org-preview-latex-fragment)

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
  '((sequence "TODO" "TOREAD" "FEEDBACK" "INPROGRESS" "WAITING" "VERIFY" "NOTE" "|" "DONE" "DELEGATED" "NOTE")))


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
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c e") 'my-insert-equation)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c a") 'my-insert-align)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c g") 'my-insert-gather)))


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



;;settings for MATLAB
(add-to-list 'load-path "~/.emacs.d/packages/matlab/matlab.el")
(load-library "matlab-load")
(add-hook 'matlab-mode-hook 'company-mode)
(add-hook 'matlab-shell-mode-hook 'company-mode)
(setq auto-mode-alist
    (cons
     '("\\.m$" . matlab-mode)
     auto-mode-alist))


;;LaTeX
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
                 (local-set-key (kbd "C-c C-g") 'writegood-grade-level)
                 (local-set-key (kbd "C-c C-e") 'writegood-reading-ease)))))
                 
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
 '(custom-enabled-themes (quote (wheatgrass)))
 '(inhibit-startup-screen t)
 '(matlab-shell-command-switches '("-nodesktop -nosplash"))
 '(org-startup-folded t)
 '(org-startup-truncated nil)
 '(org-wiki-template
   "#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  content
#+OPTIONS: \\n:t
#+OPTIONS: H:3 num:2
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/readtheorg.css\"/>
#+HTML_HEAD: <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
#+HTML_HEAD: <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
#+HTML_HEAD: <script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.js\"></script>
#+HTML_HEAD: <script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script>

- [[wiki:index][Index]]

- Related: 

* %n
"))

;;Custom Keybindings
(global-set-key (kbd "C-x g") 'magit-status)

;;Insert four spaces
(defun my-insert-four ()
  (interactive)
  (insert "    "))
(global-set-key (kbd "C-x <up>") 'my-insert-four)
                
