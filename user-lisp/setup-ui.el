;;; setup-ui.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; fonts
(use-package fontaine
  :ensure t
  :if my/graphical
  :config
  ;; The concise one which relies on "implicit fallback values"
  (setq fontaine-presets
	'((regular
	   :default-height 100)
	  ;; settinging some font for a smaller screen
	  (small-screen
	   :default-weight semilight
	   :default-height 140)
	  ;; settinging some font for a larger screen
	  (larger-screen
	   :default-weight semilight
	   :default-height 155)
	  (large
	   :default-weight semilight
	   :default-height 180
	   :bold-weight extrabold)
	  (t ; our shared fallback properties
	   :default-family "JetBrainsMono Nerd Font Mono"
	   ;; :default-family "AporeticSerifMonoNerdFont"
	   ;; :default-family "GeistMono Nerd Font"
	   :default-weight medium
	   ;; :variable-pitch-family "Merriweather"
	   ;; :variable-pitch-weight light
	   :variable-pitch-family "CMU Serif"
	   :variable-pitch-height 1.5)))
  ;; now set the preset
  ;; if is my laptop or lab machine, the screens are a bit smaller and I am sitting
  ;; a bit closer so will make the font a tad larger
  (if (or (equal (system-name) "lab") (equal (system-name) "mover"))
      (fontaine-set-preset 'small-screen)
    ;; otherwise at home with a screen that is a touch bigger
    ;; and am sitting a bit further away so make font bigger
    (fontaine-set-preset 'larger-screen)))


(setq use-default-font-for-symbols t)
;; (let ((font (font-spec :family "Iosevka Comfy Wide" :size 13.0 :weight 'normal)))
;;   (set-face-font 'default font)
;;   (set-face-font 'fixed-pitch font))
;;; Color theme
;;; my fork of Doom themes
(use-package doom-themes
  ;; :ensure (doom-themes :type git :local-repo "~/.doom.d/doom-themes")
  :ensure (doom-themes
	   :host github
	   :repo "egstatsml/doom-themes"
	   :branch "catppuccin")
  :config
  ;; the blinking cursor is a bit annoying in my opinion
  (blink-cursor-mode -1)
  ;; load theme
  (load-theme 'doom-catppuccin-mocha t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; change the pulse highlighting, they are a bit yucky and are used by
  ;; hel when I copy a text
  (custom-set-faces
   `(pulse-highlight-start-face ((t (:foreground ,(doom-color 'base0)
                                                 :background ,(doom-color 'builtin)))))))
;; Switching themes
(defun ethan/toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-catppuccin-mocha)
      (load-theme 'doom-catppuccin-latte t)
    (load-theme 'doom-catppuccin-mocha t)))


(use-package ember-theme
  :ensure (ember-theme
           :host github
           :repo "https://github.com/ember-theme/emacs")
  :init
  (add-to-list 'custom-theme-load-path
               (file-name-directory (locate-library "ember-theme")))
  (load-theme 'ember-light t))


(use-package mood-line
  :ensure (mood-line
	   :host gitlab
	   :repo "jessieh/mood-line")
  :init

  (defun mood-line-segment-modal ()
  "Changing this to have hel mode as well"
  (cond
   ((bound-and-true-p hel-mode)
    (mood-line-segment-modal--hel-fn))
   ((bound-and-true-p meow-mode)
    (mood-line-segment-modal--meow-fn))
   ((bound-and-true-p xah-fly-keys)
    (mood-line-segment-modal--xah-fn))
   ((or (bound-and-true-p god-local-mode)
        (bound-and-true-p god-global-mode))
    (mood-line-segment-modal--god-fn))))

  (defcustom mood-line-segment-modal-hel-state-alist
    '((normal . ("🅝" . font-lock-variable-name-face))
      (insert . ("🅘" . font-lock-string-face))
      (motion . ("🅜" . font-lock-constant-face)))

    "Alist specifying indicators and faces corresponding `meow-mode' states.
The face may be either a face symbol or a property list of key-value pairs;
e.g., (:foreground \"red\")."
    :group 'mood-line-segment-modal
    :type '(alist :key-type symbol
                  :value-type (cons (string :tag "Display text")
                                    (choice :tag "Face" face plist))))
  (defun mood-line-segment-modal--hel-fn ()
    "Return the current `evil-mode' state."
    (when (boundp 'hel-state)
      (let ((mode-cons (alist-get hel-state
                                  mood-line-segment-modal-hel-state-alist)))
        (concat (propertize (car mode-cons)
                            'face (cdr mode-cons))))))
  (defun mood-line-segment-current-task ()
    "Return the name of the current task clocked in.

Need to check if org-clock-current task has been defined, only occurs after
org-clock has been loaded in"
    (propertize (substring-no-properties
		 (concat " : "
			 (format-mode-line
			  (if (boundp 'org-clock-current-task)
			      org-clock-current-task nil))))
		'face 'mood-line-major-mode))
  :config
  ;; this is to fix a bug with the way some of the modules are loaded in
  ;; mood-line
  ;; https://git.tty.dog/jessieh/mood-line/issues/8
  (require 'mood-line-segment-vc)
  ;; setting my own mood line format
  ;;
  (setq mood-line-format
	(mood-line-defformat
	 :left
	 (
	  ((mood-line-segment-modal) . " ")
	  ((mood-line-segment-buffer-status) . " ")
	  ((mood-line-segment-buffer-name)   . " "))
	 ;; (mood-line-segment-major-mode))
	 :right
	 (((mood-line-segment-scroll)             . " ")
	  ((mood-line-segment-cursor-position)    . "  "))))
  ;; Enable mood-line
  (mood-line-mode))

(use-package hide-mode-line
  :ensure t
  :hook (pdf-mode . hide-mode-line-mode))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "")
  (setq dashboard-startup-banner "/home/ethan/.local/assets/xemacs_color_reg.png")
  (setq dashboard-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-footer-messages
	'("Welcome Home" "You're capable of so much"))
  ;; disable all the dashboards
  (setq dashboard-items nil))


(use-package mixed-pitch
  :ensure t
  :hook ((LaTeX-mode . mixed-pitch-mode)
	(org-mode . mixed-pitch-mode))
  :config
  (ethan/append-to-list 'mixed-pitch-fixed-pitch-faces
			'(solaire-line-number-face
			  org-date
			  org-footnote
			  org-special-keyword
			  org-property-value
			  org-ref-cite-face
			  org-tag
			  org-todo-keyword-todo
			  org-todo-keyword-habt
			  org-todo-keyword-done
			  org-todo-keyword-wait
			  org-todo-keyword-kill
			  org-todo-keyword-outd
			  org-todo
			  org-done
			  org-modern-priority
			  org-modern-tag
			  org-modern-done
			  org-modern-date-active
			  org-modern-date-inactive
			  org-modern-time-active
			  org-modern-time-inactive
			  org-drawer
			  font-lock-comment-face
			  corfu-default
			  corfu-current
			  corfu-popupinfo
			  corfu-annotations
			  corfu-deprecated
			  )))

(defun ethan/toggle-line-numbers ()
  (interactive)
  (setq display-line-numbers nil))

(setopt display-line-numbers nil)

;; perfect margins please
(use-package perfect-margin
  :ensure (perfect-margin
	   :host github
	   :repo "mpwang/perfect-margin")
  :config
  (setq perfect-margin-ignore-regexps '("^minibuf" "^[[:space:]]*\\*"))
  (setq perfect-margin-ignore-modes '(doc-view-mode nov-mode helpful-mode))
  (perfect-margin-mode))


;; viewing colours of things in emacvs
(use-package colorful-mode
  :ensure t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(provide 'setup-ui)
