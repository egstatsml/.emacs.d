;; Alot taken from https://github.com/jimm/elisp/blob/master/eshell-customize.el
;; By Jim Menard

(setq eshell-history-size 512)

(require 'em-hist)			; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version

(defun eshell/ef (fname-regexp &optional dir)
  (ef fname-regexp (or dir default-directory)))

(defun chop-path (path-list n)
  "Joins elements of PATH-LIST with \"/\". All but the last N
elements are abbreviated to their first letters."
  (cl-flet ((shorten (elm) (if (zerop (length elm)) ""
                             (substring elm 0 1))))
    (if (> (length path-list) n)
        (concat
         (mapconcat #'shorten (butlast path-list n) "/")
         "/"
         (mapconcat #'identity (last path-list n) "/"))
      (mapconcat #'identity path-list "/"))))

;; ; From http://www.emacswiki.org/cgi-bin/wiki.pl/EshellWThirtyTwo
;; ; Return nil, otherwise you'll see the return from w32-shell-execute
;; (defun eshell/open (file)
;;   "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for
;; backslashes"
;;   (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name file)))
;;   nil)

(add-hook 'eshell-mode-hook
	  (lambda ()
            (local-set-key "\C-c\C-q" 'eshell-kill-process)
            (local-set-key "\C-c\C-k" 'compile)))
