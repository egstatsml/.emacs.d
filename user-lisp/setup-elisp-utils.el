;;; setup-elisp-utils.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Utils that I use to make configuring things in elisp a bit easier
;;; Customization

;; add a bunch of items to a list
(defun ethan/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(provide 'setup-elisp-utils)
