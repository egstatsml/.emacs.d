;;;###autoload
(defun yasnippet-snippets-initialize ()
  "Load the `yasnippet-snippets' snippets directory."
  ;; NOTE: we add the symbol `yasnippet-snippets-dir' rather than its
  ;; value, so that yasnippet will automatically find the directory
  ;; after this package is updated (i.e., moves directory).
  (unless (member 'yasnippet-snippets-dir yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
    (yas--load-snippet-dirs)))

(defgroup yasnippet-snippets nil
  "Options for yasnippet setups.

This is useful for customizing options declared in
“.yas-setup.el” files.  For example, you could declare a
customizable variable used for a snippet expansion.

See Info node `(elisp)Customization Types'."
  :group 'yasnippet)

(defun yasnippet-snippets--no-indent ()
  "Set `yas-indent-line' to nil."
  (set (make-local-variable 'yas-indent-line) nil))

;;;###autoload
(eval-after-load 'yasnippet
  '(yasnippet-snippets-initialize))
