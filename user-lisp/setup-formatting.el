;;; setup-formatting.el -*- lexical-binding: t; no-byte-compile: t; -*-

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
