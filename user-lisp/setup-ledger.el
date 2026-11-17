;;; setup-ledger.el --- getting my budget together-*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ethan Goan
;;
;; Author: Ethan Goan <ethan@work>
;; Maintainer: Ethan Goan <ethan@work>
;; Created: November 15, 2026
;; Modified: November 15, 2026
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:



(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1
        ledger-mode-should-check-version nil)
  :bind (:map ledger-mode-map
              ("RET" . 'newline)
              ("C-o" . 'open-line))
  :custom
  (ledger-highlight-xact-under-point nil) ; taken from Steve Purcell
  :config

  (defun my/ledger-add ()
    "Add a transaction from known accounts"
    (interactive)
    (let ((date (ledger-read-date "Date: ")))
      (insert date " "))
    (let* ((payees (ledger-payees-list))
           (payee (completing-read "Payee: " payees nil t)))
      (insert payee "\n    "))
    (let* ((accounts (ledger-accounts-list))
           (account (completing-read "Payee Account: " accounts nil t)))
      (insert account "   ")
      (insert (read-string "Amount: " "0.00") "\n    ") ;inserting the cost
      (insert (completing-read "Expense Account: " accounts nil t) " "))
    (ledger-post-align-dwim))


(use-package flycheck-ledger
  :after ledger-mode
  :ensure t)

(provide 'setup-ledger)
;;; setup-ledger.el ends here
