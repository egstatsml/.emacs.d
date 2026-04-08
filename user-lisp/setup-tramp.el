;;; setup-tramp.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package tramp
  :ensure t
  :commands (sudo-find-file sudo-this-file)
  :bind ("C-x C-S-f" . sudo-find-file)
  :config
  ;; need to make sure terminal type is set to be a big ole dummy
  (setq tramp-terminal-type "tramp")
  ;; TODO: may want remote dir locals to be available as well, though will probably want
  ;; this to be specific to each directory/project
  (setq enable-remote-dir-locals t)
  ;; try add qsub method to tramp to log into hpc
  (add-to-list 'tramp-methods
	       ;; this is an internal method for interactive scripting, change to what your server uses
	       '("qsub"
		 (tramp-login-program        "qsub")
		 (tramp-login-args           (("-I -S /bin/bash -l select=1:ncpus=2:ngpus=1:mem=2g -l walltime=08:00:00"))) ; options here?
		 ;; the local $SHELL may contain conflicting configuration
		 ;; this should be good for most cases
		 (tramp-login-env            (("SHELL") ("/usr/local/qutsh/bin/bash")))
		 (tramp-remote-shell         "/usr/local/qutsh/bin/qutsh")
		 ;; (tramp-remote-shell-args    ("-c"))
		 (tramp-connection-timeout   10)))
  ;; settings to speed up tramp
  ;; mostly from https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (setq remote-file-name-inhibit-locks t
	tramp-use-scp-direct-remote-copying t
	remote-file-name-inhibit-auto-save-visited t)
  ;; setting out-of-band limit to be 1MB
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
	tramp-verbose 2)
  ;; using direct async processes
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)
  ;; use inline compression for smaller files over out-of-band
  ;; which will use external tools like scp/rsync
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
	tramp-verbose 2)

  ;; if I run compile commands, tramp will turn off the password storing mechanisms
  ;; want to turn it back on
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

  ;; cache a bunch more when working with tramp
  (defun memoize-remote (key cache orig-fn &rest args)
    "Memoize a value if the key is a remote path."
    (if (and key
             (file-remote-p key))
	(if-let ((current (assoc key (symbol-value cache))))
            (cdr current)
          (let ((current (apply orig-fn args)))
            (set cache (cons (cons key current) (symbol-value cache)))
            current))
      (apply orig-fn args)))

  ;; Memoize current project
  ;; TODO: I have copied this directly from the original source but I am using
  ;; projectile so won't help me much at the moment
  (defvar project-current-cache nil)
  (defun memoize-project-current (orig &optional prompt directory)
    (memoize-remote (or directory
			project-current-directory-override
			default-directory)
                    'project-current-cache orig prompt directory))

  (advice-add 'project-current :around #'memoize-project-current)

  ;; Memoize magit top level
  (defvar magit-toplevel-cache nil)
  (defun memoize-magit-toplevel (orig &optional directory)
    (memoize-remote (or directory default-directory)
                    'magit-toplevel-cache orig directory))
  (advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

  ;; memoize vc-git-root
  (defvar vc-git-root-cache nil)
  (defun memoize-vc-git-root (orig file)
    (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
      ;; sometimes vc-git-root returns nil even when there is a root there
      (when (null (cdr (car vc-git-root-cache)))
	(setq vc-git-root-cache (cdr vc-git-root-cache)))
      value))
  (advice-add 'vc-git-root :around #'memoize-vc-git-root)

  ;; memoize all git candidates in the current project
  (defvar $counsel-git-cands-cache nil)
  (defun $memoize-counsel-git-cands (orig dir)
    ($memoize-remote (magit-toplevel dir) '$counsel-git-cands-cache orig dir))
  (advice-add 'counsel-git-cands :around #'$memoize-counsel-git-cands)

  ;; got these fns from karthink
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|sudo:root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
		 (concat "/sudo:root@localhost:" file))))
  (defun sudo-this-file ()
    "Open the current file as root."
    (interactive)
    (sudo-find-file (file-truename buffer-file-name)))
  (setq remote-file-name-inhibit-cache 86400)
  (setq tramp-verbose 1)
  (with-eval-after-load 'vc
    (setq vc-ignore-dir-regexp
          (format "%s\\|%s"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))))

(require 'setup-tramp)
