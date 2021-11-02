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
(defvar my/desktop (equal (system-name) "abode"))
(defvar my/lab (equal (system-name) "lab"))
(defvar my/laptop (equal (system-name) "laptop"))
(defvar my/greenbeacon (equal (system-name) "greenbeacon"))
(defvar my/lyra (equal (user-login-name) "n9197621"))

;; Setting up directories that have additional plugins
;; this will look recursively throughout packages directory
(let ((default-directory  "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))
;; ensuring mu4e is in the path
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;setting package archives
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-archive-priorities
      '(("org" . 20)
        ("melpa" . 10)
        ("gnu" . 10)))
;; adding use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;; delete whitspace upon saving a file
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Changing where backup files are saved
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))

;; Some global settings
;; disable tool-bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;highlighting parenthesis etc.
(show-paren-mode 1)
;; linum mode
;; don't want to enable it globally, as certain buffers
;; like pdf-view, org and terminals can slow down with it on
(use-package linum-mode
  :hook (prog-mode . linum-mode))

(setq-default indent-tabs-mode nil)
;; GENERAL PACKAGE INITS
;;
;;
;; using Forge with Magit
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g" . 'magit-status))

(use-package forge
  :ensure t
  :after magit)

;; enable wakatime
(use-package wakatime-mode
  :ensure t
  :init
  (setq wakatime-api-key "3ba9ed56-aa83-4b89-b415-f272f233b61f")
  (setq wakatime-cli-path "/home/ethan/.local/bin/wakatime")
  :config
  (global-wakatime-mode t))

;; enable which-key package
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; helpful package
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; company mode for completion
(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; rainbow delimeters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; giving undo tree a go, makes things a bit nicer
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))



;; loading all the required init files
;; loading default inits, initialisations used across all my machines
(load-file "~/.emacs.d/ivy_init.el")
(load-file "~/.emacs.d/python_lsp_init.el")
(load-file "~/.emacs.d/r_init.el")
(load-file "~/.emacs.d/eshell_init.el")
(load-file "~/.emacs.d/spell_init.el")
(load-file "~/.emacs.d/term_init.el")
(load-file "~/.emacs.d/evil_init.el")
(load-file "~/.emacs.d/visuals_init.el")
(load-file "~/.emacs.d/programming_init.el")
(load-file "~/.emacs.d/kbds_init.el")

;; checking to see if additional inits used by desktop machines,
;; such as inits for email etc should be loaded
(defun load-additional-inits ()
  (if (or my/desktop my/laptop my/lab)
      (load-desktop)))

(defun load-desktop ()
  (load-file "~/.emacs.d/mu4e_init.el")
  (load-file "~/.emacs.d/latex_init.el")
  (load-file "~/.emacs.d/org_init.el")
  (load-file "~/.emacs.d/icons_init.el")
  (load-file "~/.emacs.d/calendar_init.el"))

(load-additional-inits)
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
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(beacon-color "#d33682")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("31f1723fb10ec4b4d2d79b65bcad0a19e03270fe290a3fc4b95886f18e79ac2f" "feb8e98a8a99d78c837ce35e976ebcc97abbd8806507e8970d934bb7694aa6b3" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" default))
 '(diff-hl-show-hunk-posframe-internal-border-color "#357535753575")
 '(elpy-rpc-python-command "python3")
 '(evil-digit-bound-motions '(evil-beginning-of-visual-line))
 '(evil-emacs-state-cursor '("#E57373" hbar) t)
 '(evil-insert-state-cursor '("#E57373" bar) t)
 '(evil-normal-state-cursor '("#FFEE58" box) t)
 '(evil-visual-state-cursor '("#C5E1A5" box) t)
 '(exwm-floating-border-color "#E1DBCD")
 '(fci-rule-color "#073642")
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(frame-background-mode 'dark)
 '(helm-minibuffer-history-key "M-p")
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors '(("#ed0547ad8099" . 0) ("#424242" . 100)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(matlab-shell-command-switches '("-nodesktop -nosplash"))
 '(mlscroll-in-color "#56bc56bc56bc")
 '(mlscroll-out-color "#424242")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#dc322f")
 '(org-agenda-files
   '("~/org/wiki/roam/20210929003500-pdmps.org" "~/org/wiki/gtd.org" "~/org/wiki/tickler.org" "~/org/wiki/someday.org" "~/org/wiki/inbox.org"))
 '(org-directory "~/org/wiki/roam")
 '(org-src-block-faces 'nil)
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
   '(org-super-agenda modus-themes wucuo multi-vterm sudo-edit mu4e-marker-icons org-noter-pdftools evil-mu4e vterm speed-type ivy-bibtex 0blayout org-noter evil-collection evil rainbow-delimiters helpful dap-mode lsp-ivy all-the-icons-ivy doom-modeline doom-themes which-key counsel-projectile projectile org-journal lsp-python-ms calfw ivy-prescient prescient wgrep counsel all-the-icons-ivy-rich ivy-rich ivy helm-ls-git helm-org all-the-icons use-package org-kanban org-roam org-roam-bibtex languagetool ess jupyter pdf-tools pdf-view-restore org-bullets color-theme color-theme-sanityinc-solarized apropospriate-theme color-theme-sanityinc-tomorrow zenburn-theme flycheck flycheck-cython flycheck-julia async-await magic-latex-buffer px ein elpy forge cmake-mode wakatime-mode matlab-mode htmlize ghub mu4e-alert mu4e-conversation mu4e-jump-to-list mu4e-maildirs-extension mu4e-query-fragments ebib xref-js2 writegood-mode stan-mode org-wiki markdown-mode magit langtool helm-bibtex excorporate ess-view ess-smart-underscore ess-smart-equals ess-R-data-view auto-complete-auctex ac-html))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#3a933a933a93")
 '(pos-tip-foreground-color "#9E9E9E")
 '(python-indent-offset 2)
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.office365.com")
 '(smtpmail-smtp-service 25)
 '(tabbar-background-color "#357535753575")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-python-bin nil)
 '(window-divider-mode nil)
 '(xterm-color-names
   ["black" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#972500" "#315b00" "#70480f" "#2544bb" "#8f0075" "#30517f" "white"]))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "ADBO" :family "FiraCode Nerd Font Mono"))))
 '(fixed-pitch ((t (:family "FiraCode Nerd Font Mono"))))
 '(org-document-title ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif" :height 1.4 :underline nil))))
 '(org-done ((t (:foreground "PaleGreen" :strike-through t))))
 '(org-headline-done ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif" :strike-through t))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif" :height 1.2))))
 '(org-level-2 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif" :height 1.0))))
 '(org-level-3 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif" :height 1.0))))
 '(org-level-4 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif"))))
 '(org-level-5 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :foreground "#d8d8d8" :family "Sans Serif"))))
 '(variable-pitch ((t (:family "ETBembo")))))
;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
;;  '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
