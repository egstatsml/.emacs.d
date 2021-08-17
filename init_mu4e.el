(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;;(add-to-list 'load-path "~/.emacs.d/mu4e/")
(require 'mu4e)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
