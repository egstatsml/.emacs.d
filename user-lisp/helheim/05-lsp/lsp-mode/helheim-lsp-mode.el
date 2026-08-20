;;; helheim-lsp-mode.el -*- lexical-binding: t; no-byte-compile: t -*-

(setup lsp-mode
  (:install t)
  (:setopt lsp-keymap-prefix "C-c l"
           lsp-log-io nil
           lsp-enable-indentation t
           lsp-enable-imenu t
           lsp-file-watch-threshold 1000)
  (:after-load
    (lsp-enable-which-key-integration)
    (:hook hel-insert-state-exit-hook +lsp-close-signature)
    (:keymap lsp-mode-map
      (:bind :state normal
        "K"     'lsp-describe-thing-at-point
        "M"     'lsp-describe-thing-at-point)
      (:bind
        "C-c l" (cons "LSP" lsp-command-map)))))

(defun +lsp-close-signature ()
  "Close the displayed `lsp-signature'."
  (when lsp-signature-mode
    (lsp-signature-stop)
    t))

;;; lsp booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ; or lsp-deferred

(with-eval-after-load 'lsp-mode
  (defgroup lsp-python-refly nil
    "LSP support for Python(pyrefly)."
    :group 'lsp-mode
    :link '(url-link "https://pyrefly.org"))

  (defcustom lsp-python-refly-clients-server-command '("pyrefly" "lsp")
    "Command to start the python pyrefly language server."
    :group 'lsp-python-refly
    :risky t
    :type '(repeat string))


  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-python-refly-clients-server-command))
                    :activation-fn (lsp-activate-on "python")
                    :priority -1
                    :add-on? t
                    :server-id 'py-refly))

  (lsp-consistency-check lsp-python-refly))

(setup lsp-ui
  (:install t)
  (:setopt lsp-ui-doc-enable t
           lsp-ui-doc-position 'at-point
           lsp-ui-sideline-enable t
           lsp-ui-peek-enable t
           lsp-ui-imenu-enable t
           lsp-ui-flycheck-enable t))

(setup consult-lsp
  (:install t)
  (:after lsp-mode)
  (:keymap lsp-mode-map
    (:bind [remap xref-find-apropos] 'consult-lsp-symbols)))

;;; .
(provide 'helheim-lsp-mode)
;;; helheim-lsp-mode.el ends here
