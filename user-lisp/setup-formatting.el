;;; setup-formatting.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package apheleia
  :ensure (apheleia
	   :host github
	   :repo "radian-software/apheleia")
  :custom
  (apheleia-remote-algorithm 'remote)
  :config
  ;; setting formatters
  ;; formatting with ruff with pixi
  ;; needed at the moment, Pet not finding pixi variables properly
  (add-to-list 'apheleia-formatters '(pixi-ruff "pixi" "run" "ruff" "format" "--silent"
                                                (apheleia-formatters-fill-column "--line-length") "--stdin-filename"
                                                filepath "-"))
  (add-to-list 'apheleia-formatters '(pixi-ruff-isort "pixi" "run" "ruff" "check" "-n" "--select" "I" "--fix" "--fix-only"
                                                      "--stdin-filename" filepath "-"))
  ;; latex
  (setf (alist-get 'latexindent apheleia-formatters)
	'("latexindent" "-y=/home/ethan/.config/latexindent/mysettings.yaml" "--logfile=/dev/null"))
  ;; elisp with elisp-autofmt
  ;; from https://github.com/radian-software/apheleia/issues/272
  (add-to-list
   'apheleia-formatters
   '(elisp-autofmt
     .
     (
      ;; Evaluates to the main external formatter command that would be run (synchronously) by
      ;; `elisp-autofmt-buffer', as a list of strings. This expression will be evaluated whenever
      ;; apheleia runs the formatter, and "flattened" into the main command definition.
      ;;
      ;; The logic to build the formatting command is rather complicated, so we extract it directly
      ;; from elisp-autofmt by advising some internals and running a "no-op" formatting pass.
      ;;
      ;; The formatting command executes a python script, with a number of flags (to set line width,
      ;; etc) based on values from the current buffer. It reads the source from stdin and outputs
      ;; the formatted contents to stdout, as expected by apheleia by default.
      (let ((command nil))
        (require 'elisp-autofmt)
        (elisp-autofmt--with-advice
            ((
              ;; elisp-autofmt uses this to (synchronously) invoke formatting and other processes.
              #'elisp-autofmt--call-process
              :around
              ;; Intercept the formatting command from elisp-autofmt and store it in the local `command'
              ;; variable.
              (lambda (orig-fn proc-id command-with-args stdin-buffer stdout-buffer)
                ;; elisp-autofmt also sometimes needs to generate cache files via external commands. We
                ;; don't want to interfere with those processes; they need to run before the main
                ;; apheleia formatter command can succeed.
                ;;
                ;; Check if this is the formatting process by looking for a python command which doesn't
                ;; include a "--gen-defs" argument.
                (if (and (string-equal (car command-with-args) elisp-autofmt-python-bin)
                         (not (member "--gen-defs" command-with-args)))
                    (progn
                      ;; Store the command so we can pass it to apheleia.
                      (setq command command-with-args)
                      ;; Fake a "successfully executed" formatting run (response code 2 as expected by
                      ;; elisp-autofmt, and no stderr output).
                      '(2 . nil))
                  ;; If this isn't the formatting process (i.e. it's a caching process), just let it run
                  ;; normally. This should occur pretty infrequently.
                  ;;
                  ;; Note these cache commands sometimes fail with broken pipe errors, even when using
                  ;; `elisp-autofmt-buffer' directly. But the underlying processes do get triggered, and
                  ;; the formatter seems to fix itself after a few attempts.
                  (funcall orig-fn proc-id command-with-args stdin-buffer stdout-buffer))))
             ;; Don't let elisp-autofmt actually write to the buffer at this point, as it
             ;; would fill it with empty output.
             (#'elisp-autofmt--replace-region-contents-wrapper :override (lambda (&rest _))))
          ;; Now perform a fake formatting pass to intercept/store the formatting command. This
          ;; should be fast, since the formatting process isn't actually getting run (though note
          ;; that it's possible for cache processes to be run synchronously here, which may cause
          ;; temporary broken pipe issues as described above).
          (elisp-autofmt-buffer)
          (unless (listp command)
            (error "Could not determine elisp-autofmt formatting command"))
          ;; Return the extracted command; apheleia will recognize it as a list of arguments.
          command))
      ;; elisp-autofmt normally forces a non-zero exit code for the formatter's success case, which
      ;; is handled internally during elisp-autofmt processing. For apheleia we need to restore sane
      ;; exit code behavior.
      "--exit-code=0")))
  ;; some functions to automatically format a project
  ;; relies on projectile
  (defun my/format-file (project-dir-root file)
    "Format a file with Apheleia"
    ;; I run this on remote machines and don't want to accidentally
    ;; open an image on remote machine
    (when (not (or (file-name-extension "jpg") (file-name-extension "png")))
      (find-file (concat project-dir-root file))
      (if apheleia-formatter
	  (apheleia-format-buffer apheleia-formatter))))

  (defun my/format-project (&optional ff-variant)
    "Format all files in a project"
    (interactive "P")
    (projectile-maybe-invalidate-cache nil)
    (let* ((project-root (projectile-acquire-root))
	   (project-root-dir (projectile-project-root))
	   (files
	    (projectile-project-files project-root)))
      (dolist (file files)
	(my/format-file project-root-dir file)))))

(provide 'setup-formatting)
