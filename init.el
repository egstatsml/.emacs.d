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
(defvar machine_type "desktop")

(defvar my/desktop (equal (system-name) "abode"))
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
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

;; (with-eval-after-load 'magit
;;   (require 'forge))


;; Changing where backup files are saved
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))



;; Some global settings

;; disable tool-bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
;; want to default split to vertical
;;(setq split-width-threshold 0)
;;(setq split-height-threshold nil)
;; new keybindings for windmove
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)


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

;; enable wakatime
(setq wakatime-api-key "3ba9ed56-aa83-4b89-b415-f272f233b61f")
(setq wakatime-cli-path "/home/ethan/.local/bin/wakatime")
(global-wakatime-mode)
;;

;;enable column-enforce mode for sorce code modes
(require 'column-enforce-mode)
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


;; enable which-key package
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;; enable doom modeline
;; want with all-the-icons as well
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; helpful package
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; lsp-mode
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; treemacs to get a nicer display of error messages
(use-package lsp-treemacs
  :after lsp)

;; lsp-ivy 
(use-package lsp-ivy)


;; enable dap-mode
(use-package dap-mode)
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  ;; :config
  ;; ;; Set up Node debugging
  ;; (require 'dap-node)
  ;; (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  
  ;; ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger")))


;;company mode for completion
(use-package company
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
  :hook (company-mode . company-box-mode))


;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; rainbow delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



;; loading all the required init files
;;(load "flymake")
(defun load-inits (machine_type)
  (if (string-equal machine_type "desktop")
      (load-desktop)
    ;; otherwise just load terminal packages
    (load-terminal)))
;; loading desktop packages
(defun load-desktop ()
  (load-file "~/.emacs.d/mu4e_init.el")
  (load-file "~/.emacs.d/org_init.el")
  (load-file "~/.emacs.d/latex_init.el")
  (load-file "~/.emacs.d/matlab_init.el")
  (load-file "~/.emacs.d/python_lsp_init.el")
  ;;(load-file "~/.emacs.d/python_init.el")
  ;;(load-file "~/.emacs.d/ipython_init.el")
  (load-file "~/.emacs.d/r_init.el")
  (load-file "~/.emacs.d/ivy_init.el")
  (load-file "~/.emacs.d/icons_init.el")
  ;;(load-file "~/.emacs.d/flymake_init.el")
  (load-file "~/.emacs.d/eshell_init.el"))
;; loading terminal packages
(defun load-terminal ()
  (load-file "~/.emacs.d/org_init.el")
  (load-file "~/.emacs.d/ivy_init.el")
  ;; (load-file "~/.emacs.d/python_init.el")
  (load-file "~/.emacs.d/python_lsp_init.el")
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
 '(beacon-color "#d33682")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" default))
 '(elpy-rpc-python-command "python3")
 '(evil-emacs-state-cursor '("#E57373" hbar) t)
 '(evil-insert-state-cursor '("#E57373" bar) t)
 '(evil-normal-state-cursor '("#FFEE58" box) t)
 '(evil-visual-state-cursor '("#C5E1A5" box) t)
 '(fci-rule-color "#073642")
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors '(("#ed0547ad8099" . 0) ("#424242" . 100)))
 '(inhibit-startup-screen t)
 '(matlab-shell-command-switches '("-nodesktop -nosplash"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-directory "~/org/wiki/roam")
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
   '(speed-type ivy-bibtex 0blayout org-noter evil-collection evil rainbow-delimiters helpful dap-mode lsp-ivy all-the-icons-ivy doom-modeline doom-themes which-key counsel-projectile projectile org-journal lsp-python-ms calfw ivy-prescient prescient wgrep counsel all-the-icons-ivy-rich ivy-rich ivy helm-ls-git helm-org all-the-icons use-package org-kanban org-roam org-roam-bibtex languagetool ess jupyter pdf-tools pdf-view-restore org-bullets color-theme color-theme-sanityinc-solarized apropospriate-theme color-theme-sanityinc-tomorrow zenburn-theme flycheck flycheck-cython flycheck-julia async-await magic-latex-buffer px ein elpy forge cmake-mode wakatime-mode matlab-mode htmlize ghub mu4e-alert mu4e-conversation mu4e-jump-to-list mu4e-maildirs-extension mu4e-query-fragments ebib xref-js2 writegood-mode stan-mode org-wiki markdown-mode magit langtool helm-bibtex excorporate ess-view ess-smart-underscore ess-smart-equals ess-R-data-view auto-complete-auctex ac-html))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#3a933a933a93")
 '(pos-tip-foreground-color "#9E9E9E")
 '(python-indent-offset 2)
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.office365.com")
 '(smtpmail-smtp-service 25)
 '(tabbar-background-color "#357535753575")
 '(vc-annotate-background nil)
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
 '(window-divider-mode nil))




(require 'calfw)

(setq excorporate-configuration (quote ("n9197621@qut.edu.au" . "https://outlook.office365.com/EWS/Exchange.asmx")))


(setq excorporate-calendar-show-day-function 'exco-calfw-show-day)
