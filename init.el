;; init.el

;; Author: Ethan Goan (With a huge amount of help from the ELisp community)

;; Description:
;; My configuration file for Emacs, where I have focused on configuring it for
;; Python, LaTeX, org-mode and Matlab

;; I run Emacs on many different machines (some with a graphical display and
;; without). Some modes are suitable for different environments (ie. if im on a
;; cluster without a display I'm probably not using LaTeX packages). To identify which
;; packages are to be loaded, it is required to first set the variable "machine_type"
;; This will determine which settings are loaded for the different environments



;; Setting the machine type
;; This MUST be done
;; The only valid values for this variable are "desktop"
;; or "terminal"
(defvar machine_type "terminal")

;; Setting up directories that have additional plugins
;; this will look recursively throughout packages directory
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

;; Custom Keybindings
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


;; loading all the required init files
;;(load "flymake")
(defun load-inits (machine_type)
  (if (string-equal machine_type "desktop")
      (load-desktop)
    ;; otherwise just load terminal packages
    (load-terminal)))
;; loading desktop packages
(defun load-desktop ()
       (load-file "~/.emacs.d/org_init.el")
       (load-file "~/.emacs.d/latex_init.el")
       (load-file "~/.emacs.d/matlab_init.el")
       (load-file "~/.emacs.d/python_init.el")
       (load-file "~/.emacs.d/ipython_init.el")
       (load-file "~/.emacs.d/r_init.el")
       ;;(load-file "~/.emacs.d/flymake_init.el")
       (load-file "~/.emacs.d/eshell_init.el"))
;; loading terminal packages
(defun load-terminal ()
  (load-file "~/.emacs.d/org_init.el")
     (load-file "~/.emacs.d/python_init.el")
     (load-file "~/.emacs.d/r_init.el")
     ;;(load-file "~/.emacs.d/flymake_init.el")
     (load-file "~/.emacs.d/eshell_init.el"))

(load-inits machine_type)


;; Some of my other custom set variables
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
    (flycheck-cython flycheck-julia async-await magic-latex-buffer tramp px ein elpy forge cmake-mode wakatime-mode matlab-mode htmlize ghub mu4e-alert mu4e-conversation mu4e-jump-to-list mu4e-maildirs-extension mu4e-query-fragments ebib xref-js2 writegood-mode stan-mode org-wiki markdown-mode magit langtool helm-bibtex excorporate ess-view ess-smart-underscore ess-smart-equals ess-R-data-view auto-complete-auctex ac-html)))
 '(python-indent-offset 2)
 '(wakatime-python-bin nil))

