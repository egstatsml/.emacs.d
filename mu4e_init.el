;; ;; This buffer is for text that is not saved, and for Lisp evaluation.
;; ;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; (use-package mu4e
;;   :ensure nil
;;   ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
;;   ;; :defer 20 ; Wait until 20 seconds after startup
;;   :config

;;   ;; This is set to 't' to avoid mail syncing issues when using mbsync
;;   (setq mu4e-change-filenames-when-moving t)

;;   ;; Refresh mail using isync every 10 minutes
;;   (setq mu4e-update-interval (* 10 60))
;;   (setq mu4e-get-mail-command "mbsync -a")
;;   (setq mu4e-maildir "~/.mail/staff")

;;   (setq mu4e-drafts-folder "/Drafts")
;;   (setq mu4e-sent-folder   "/Sent Items")
;;   (setq mu4e-refile-folder "/All")
;;   (setq mu4e-trash-folder  "/Trash"))

;; (setq smtpmail-smtp-server "smtp.office365.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-stream-type  'starttls)



(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  
  (setq mu4e-contexts
        (list
         ;; Work account
         (make-mu4e-context
          :name "staff"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/staff" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "goane@qut.edu.au")
                  (user-full-name    . "Ethan Goan")
                  (mu4e-drafts-folder  . "/staff/Drafts")
                  (mu4e-sent-folder  . "/staff/Sent Items")
                  (mu4e-refile-folder  . "/staff/All")
                  (mu4e-trash-folder  . "/staff/Trash")
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (smtpmail-smtp-user . "goane@qut.edu.au")
                  (mu4e-maildir-shortcuts . (("/staff/INBOX" . ?i)
                                             ("/Archive" . ?a)
                                             ("/Sent" . ?s)
                                             ("/Trash" . ?t)
                                             ("/Junk" . ?j)))))
         
         ;; Personal account
         (make-mu4e-context
          :name "hdr"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/hdr" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "n9197621@qut.edu.au")
                  (user-full-name    . "Ethan Goan")
                  (mu4e-drafts-folder  . "/hdr/Drafts")
                  (mu4e-sent-folder  . "/hdr/Sent Items")
                  (mu4e-refile-folder  . "/hdr/All")
                  (mu4e-trash-folder  . "/hdr/Trash")
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (smtpmail-smtp-user . "n9197621@qut.edu.au")
                  (mu4e-maildir-shortcuts . (("/hdr/INBOX" . ?i)
                                             ("/Archive" . ?a)
                                             ("/Sent" . ?s)
                                             ("/Trash" . ?t)
                                             ("/Junk" . ?j))))))))

;; Notifications for incoming mail
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
