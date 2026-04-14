;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Fonts
;;
;; Set up fonts before anything else so error messages during startup were
;; readable.
;;
;; Place cursor before character and press "ga" to see information about it.
;; Press "<F1> k ga" to find out which command is bound to "ga".


;;; My constants
;; All of my bib databases.
(defconst ethan/bib-libraries (list "~/.local/ref.bib"))
;; The main db is always the first
(defconst ethan/main-bib-library (nth 0 ethan/bib-libraries))
;; PDFs directories in a list
(defconst ethan/main-pdfs-library-paths `("~/OneDrive/pdfs/"))
;; Main PDFs directory
(defconst ethan/main-pdfs-library-path (nth 0 ethan/main-pdfs-library-paths))
;; I use org-roam to manage all my notes, including bib notes.
(defconst ethan/bib-notes-dir "~/org/roam")


(when (< emacs-major-version 31)
  (load-file (expand-file-name "prepare-user-lisp.el" user-lisp-directory))
  (prepare-user-lisp))


(require 'helheim-elpaca)
;; small emacs lisp functions
;; loading early as some of these are used to write my config
(require 'setup-elisp-utils)
;;; org-mode latex preview
;; following advice from karthink, putting very early just clone of it
;; these needs to be done pretty much before anything else, otherwise will
;; use built in org
(use-package org
  :if my/graphical
  :defer
  :ensure (org :repo "https://code.tecosaur.net/tec/org-mode.git"
               :branch "dev"))

;; making sure exec-path-from shell is super early in config
(use-package exec-path-from-shell
  :ensure t
  :init
  ;; this requires that I have set my shell paths correctly such that
  ;; it is all visible in a non-interactive shell.
  ;; If this is not the case, none of it will work.
  ;; See README for info.
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  ;; when launcing standalone
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  ;; when as a daemon
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(require 'setup-emacs)
(require 'setup-tramp)
(require 'helheim-core)
(require 'helheim-tree-sitter)
(require 'cl-macs)

;;; ui
(require 'setup-ui)
;;; Other modules
(require 'helheim-xref)     ; Go to defenition framework
(require 'helheim-ibuffer)  ; Buffers menu
;; (require 'helheim-dired)    ; File-manager
;;(require 'helheim-outline-mode) ; See "Outline Mode" in Emacs manual.
(require 'helheim-window)    ; window utilities
(require 'helheim-i3)    ; i3
(require 'setup-editing) ; editing/writing

;;; Search and completion
(require 'helheim-corfu)    ; Code completion menus
(require 'helheim-vertico)  ; Emacs version of command pallet
(require 'helheim-consult)  ; A set of search commands with preview
(require 'helheim-deadgrep) ; Interface to Ripgrep
(require 'helheim-embark)   ; Context-aware action menus

;;; Major modes
(require 'helheim-emacs-lisp)
(require 'helheim-markdown)
(require 'helheim-python)
(require 'setup-latex)
;; tools
(require 'helheim-flycheck-lsp)
(require 'setup-projectile)
(require 'setup-vundo)
(require 'setup-formatting)
(require 'setup-bib)
(require 'setup-ai)
(require 'setup-term)
;;; Org mode

(when my/graphical
  ;; The `org-directory' variable must be set before `helheim-org' is loaded!
  (setopt org-directory (expand-file-name "~/notes/"))
  ;; Which modules to load. Place cursor on variable and press "M" to see
  ;; all possible values.
  (setq org-modules '(ol-bibtex ol-docview ol-info))
  (require 'helheim-org)
  (require 'helheim-org-node)
  (require 'helheim-daily-notes)
  (require 'helheim-agenda)
  (require 'setup-org-noter))
;;; Version control system
(require 'helheim-magit)
(require 'helheim-diff-hl)

;;; Keybindings
(require 'hel-leader)
(require 'helheim-keybindings)
(require 'helheim-disable-isearch)

;;; init.el ends here
