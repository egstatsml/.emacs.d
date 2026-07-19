;;; setup-notmuch.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package notmuch
  :ensure t
  :config
  ;; (setq notmuch-command "/usr/bin/notmuch")
  (setq notmuch-tagging-keys '(("a" notmuch-archive-tags "Archive")
                               ("u" notmuch-show-mark-read-tags "Mark read")
                               ("f" ("+flagged") "Flag")
                               ("s" ("+action-spam") "Mark as spam")
                               ("d" ("+action-delete") "Delete"))
        notmuch-archive-tags '("+action-archive")
        notmuch-draft-tags '("+drafts")
        mg-notmuch-deleted-tags "action-delete")

  ;; taken from prot
  (setq notmuch-show-logo nil
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-show-all-tags-list t)
  ;; search
  ;; taken from prot
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ;; ;; NOTE 2022-09-19: I disable this because I add a cosmeic
          ;; ;; emoji via `notmuch-tag-formats'.  This way I do not get
          ;; ;; an intense style which is very distracting when I filter
          ;; ;; my mail to include this tag.
          ;;
          ;; ("flag" . notmuch-search-flagged-face)
          ;;
          ;; Using `italic' instead is just fine.  Though I also tried
          ;; it without any face and I was okay with it.  The upside of
          ;; having a face is that you can identify the message even
          ;; when the window is split and you don't see the tags.
          ("flag" . italic)))
  
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        `(( :name " inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "󰺻 all unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))))
  ;; tags.
  ;; taken from prot
  (setq notmuch-message-replied-tags '("+replied")
        notmuch-message-forwarded-tags '("+forwarded")
        notmuch-show-mark-read-tags '("-unread")
        notmuch-draft-tags '("+draft")
        notmuch-draft-folder "drafts"
        notmuch-draft-save-plaintext 'ask)

  ;; email composition
  ;; taken from prot
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil)
  (setq notmuch-address-command 'internal)
  (setq notmuch-address-use-company nil)
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function nil)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                "pi[èe]ce\s+jointe?\\|"
                "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))

  (with-eval-after-load 'message
    (defun prot-notmuch-message-tab ()
      "Override for `message-tab' to enforce header line check.
More specifically, perform address completion when on a relevant header
line, because `message-tab' sometimes (not sure when/how) fails to do
that and instead tries to complete against dictionary entries."
      (interactive nil message-mode)
      (cond
       ((save-excursion
          (goto-char (line-beginning-position))
          (looking-at notmuch-address-completion-headers-regexp))
        (notmuch-address-expand-name)
        ;; Completion was performed; nothing else to do.
        nil)
       (message-tab-body-function (funcall message-tab-body-function))
       (t (funcall (or (lookup-key text-mode-map "\t")
                       (lookup-key global-map "\t")
                       'indent-relative)))))

    (advice-add #'message-tab :override #'prot-notmuch-message-tab))
  ;; reading messages
  ;; again from prot
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count))

  ;; hooks and bindings from prot
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check) ; also see `notmuch-mua-attachment-regexp'
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode) ; Check my `lin' package

  ;; empty subject check
  ;; https://notmuchmail.org/emacstips/#index27h2
  (defun my-notmuch-mua-empty-subject-check ()
    "Request confirmation before sending a message with empty subject"
    (when (and (null (message-field-value "Subject"))
               (not (y-or-n-p "Subject is empty, send anyway? ")))
      (error "Sending message cancelled: empty subject.")))
  (add-hook 'message-send-hook 'my-notmuch-mua-empty-subject-check))

(provide 'setup-notmuch)
