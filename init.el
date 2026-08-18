;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Fonts
;;
;; Set up fonts before anything else so error messages during startup
;; were readable.
;;
;; Press "ga" to see information about the character at the position, font used,
;; faces, overlays, etc. (Press "<F1> k ga" to find out which command is bound
;; to "ga".)

(let* ((font "JetBrainsMono Nerd Font Mono") ;; <- replace with you font
       (spec (font-spec :family font :size 13.0 :weight 'normal)))
  (set-face-font 'default spec)
  (set-face-font 'fixed-pitch spec)
  ;; Prepend our font to the default fontset to make it the first fallback
  ;; candidate for itself. This matters when text is bold or italic and the
  ;; default font lacks glyphs for those styles but does provide them for the
  ;; regular style. With this change, Emacs will use the regular glyphs from
  ;; the default font when bold or italic variants are unavailable, instead of
  ;; falling back to a different font.
  ;;   BUG: Using `font-spec' with `set-fontset-font' doesn't work, despite
  ;; documentation claims it is.
  (set-fontset-font t 'unicode font nil 'prepend))

(require 'cl-macs)

(cl-defun helheim-set-fontset-font (font charsets &key (fontset t) add)
  "Force some code point diapasons to use particular FONT."
  (declare (indent 1))
  (dolist (charset charsets)
    (set-fontset-font fontset charset font nil add)))

(helheim-set-fontset-font "Symbols Nerd Font Mono"
                          '((#xe5fa . #xe6b7) ;; Seti-UI + Custom  
                            (#xe700 . #xe8ef) ;; Devicons  
                            (#xed00 . #xf2ff) ;; Font Awesome  
                            (#xe200 . #xe2a9) ;; Font Awesome Extension  
                            (#xe300 . #xe3e3) ;; Weather  
                            (#xf400 . #xf533) #x2665 #x26A1 ;; Octicons   ♥ ⚡
                            (#x23fb . #x23fe) #x2b58 ;; IEC Power Symbols ⏻ ⏾ ⭘
                            (#xf300 . #xf381) ;; Font Logos   
                            (#xe000 . #xe00a) ;; Pomicons  
                            (#xea60 . #xec1e) ;; Codicons  
                            (#x276c . #x2771) ;; Heavy Angle Brackets ❬ ❱
                            (#xee00 . #xee0b) ;; Progress  
                            (#xf0001 . #xf1af0))) ;; Material Design Icons 󰀁 󱫰

;; In the modeline, we’re not restricted by a rigid grid, and non-monospace
;; Powerline symbols look better.
(helheim-set-fontset-font "Symbols Nerd Font"
                          `(;; Powerline Symbols
                            (#xe0a0 . #xe0a2) ;;  
                            (#xe0b0 . #xe0b3) ;;  
                            ;; Powerline Extra Symbols
                            (#xe0b4 . #xe0c8) ;;  
                            (#xe0cc . #xe0d7) ;;  
                            #xe0a3 #xe0ca))   ;;  

;;; my setup
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


;;; Helheim core

;; In case you use VPN. Also Emacs populates `url-proxy-services' variable
;; from: `https_proxy', `socks_proxy', `no_proxy' environment variables.
;; (setopt url-proxy-services '(("socks" . "127.0.0.1:10808")
;;                              ("https" . "127.0.0.1:10809"))
;;         gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq helheim-package-manager 'elpaca) ;; or 'straight
(require 'helheim-core)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;; (use-package org
;;   :if my/graphical
;;   :ensure (org :repo "https://code.tecosaur.net/tec/org-mode.git"
;;                :branch "dev"))
;; (elpaca-wait)
;;; Visuals
(require 'setup-ui)
;;; Essentials

(require 'helheim-minibuffer) ; Vertico + Marginalia
(require 'helheim-completion) ; Corfu + Orderless + Cape
(require 'helheim-search)     ; Consult + Deadgrep
(require 'helheim-keybindings)

(require 'helheim-ibuffer)    ; Buffers menu
(require 'helheim-dired)      ; File-manager
(require 'helheim-embark)     ; Context-aware action menus
;; (require 'helheim-modeline)   ; Normal people call this "status line"
(require 'helheim-outline)    ; See "Outline Mode" in Emacs manual

;;; Search

(require 'helheim-consult)  ; A set of search commands with preview
(require 'helheim-deadgrep) ; Interface for Ripgrep in Emacs

;;; IDE

(require 'setup-project)
(require 'helheim-xref)     ; Go to definition framework

;; (require 'helheim-eglot)    ; eglot + flymake (both built-in)
;; or
(require 'helheim-flycheck) ; Diagnostics
(require 'helheim-lsp-mode) ; Lsp-mode

;;; Version control

(require 'helheim-magit)
(require 'helheim-diff-hl)  ; git gutter indicators

;;; Org mode

;; These variables must be set before `org' is loaded!
(setopt org-directory (expand-file-name "~/org/")
        ;; Which modules to load.
        ;; Place cursor on variable and press "M" to see all possible values.
        org-modules '(ol-bibtex ol-docview ol-info))

(require 'helheim-org)
(require 'helheim-org-node)
(require 'helheim-daily-notes)

;;; Major modes

(require 'helheim-cpp)
(require 'helheim-emacs-lisp)
(require 'helheim-json)
(require 'helheim-markdown)
(require 'helheim-lua)
(require 'helheim-sh)
(require 'setup-python)
;;; Terminal emulators

(require 'helheim-ghostel) ; based on libghostty (Zig) -- same as in Ghostty
                                        ; (require 'helheim-vterm)   ; based on libvterm (C) -- same as in Neovim

;;; LLM

;; (require 'helheim-agent-shell)
                                        ; (require 'helheim-mcp-server)
(require 'setup-ai)
;;; Extra ficilities

                                        ; (require 'helheim-browser) ; Synchronize online text editor with Emacs buffer
                                        ; (require 'helheim-notmuch) ; Notmuch email client
                                        ; (require 'helheim-whisper) ; Speech to text conversion
                                        ; (require 'helheim-edit-indirect) ; Alternative "zn" binding

;;; init.el ends here
