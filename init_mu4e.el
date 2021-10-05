(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;;(add-to-list 'load-path "~/.emacs.d/mu4e/")
(require 'mu4e)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; adding mu4e markers
(use-package mu4e-marker-icons
  :ensure t
  :init (mu4e-marker-icons-mode 1))
