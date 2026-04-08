;;; setup-emacs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; this is for setting up general things that are either built in to
;;; Emacs, or are just really fundamental to some of the editing experience.
;;; Code
;; maintain list of recently accessed files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 300) ; default is 20
  (setq recentf-auto-cleanup 'mode))


;; remember location of file when reopening
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode)
  (setq save-place-file (expand-file-name "saveplace" ethan/cache-dir))
  (setq save-place-limit 600))


(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-file (expand-file-name "savehist" ethan/cache-dir))
  ;; string fonts/overlays from savehist before saving
  ;; from doom, avoids bloat
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring)))))
  (setq savehist-save-minibuffer-history t
	savehist-autosave-interval nil     ; save on kill only
	savehist-additional-variables
	'(kill-ring                        ; persist clipboard
	  register-alist                   ; persist macros
	  mark-ring global-mark-ring       ; persist marks
	  search-ring regexp-search-ring))) ; persist searches


;; better defaults
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
;; which collected better defaults from a lot of other places

;; disable bidirectional text scanning, as I only write English/code
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; skip fontification (syntax highlighting) on input
(setq redisplay-skip-fontification-on-input t)

;; don't render cursors in  non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; save the clipboard before killing
;; paste with M-y if needed
(setq save-interprogram-paste-before-kill t)
;; no duplicates in the kill ring
(setq kill-do-not-save-duplicates t)

;; better emacs regex, means I won't need to double escape everything
(setq reb-re-syntax 'string)

(provide 'setup-emacs)
;;; setup-emacs.el ends here

