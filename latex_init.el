;; LaTeX

;;defaulting to biblatex dialect
(setq-default bibtex-dialect 'biblatex)
(setq TeX-parse-self t)
;;make it so we have to specify the main file whenever creating a TeX file
(setq-default TeX-master nil)
;; set to pdfmode so that is uses pdflatex by default
(setq TeX-PDF-mode t)
;;enable flyspell and run it on all LaTeX buffers
(add-hook  'LaTeX-mode-hook 'flyspell-mode)
(add-hook  'LaTeX-mode-hook 'flyspell-buffer)
;;setting up company mode for auxtex
;;(require 'company-auctex)
;;(company-auctex-init)
(add-hook  'LaTeX-mode-hook 'company-mode)
;; adding RefTeX support
(require `reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; adding flymake to check my syntax on the fly
(require 'flymake)
(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
;; using Latexmk as the default latex command to compile
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(setq TeX-command-default "LatexMk")
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; setting the viewing program
;; setting to qpdfview on Manjaro
(with-eval-after-load "tex"
  (add-to-list 'TeX-view-program-list '("evince" "/usr/bin/evince %o"))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("evince")))
;;for langtool
(setq langtool-language-tool-jar "~/.emacs.d/packages/LanguageTool-4.1/languagetool-commandline.jar")
(require 'langtool)

;;config after we load a LaTeX doc.
(eval-after-load "latex"
  '(progn
     (add-hook 'LaTeX-mode-hook
               (lambda ()
                 ;;setting up keys for ebib
                 (local-set-key (kbd "C-c e") 'ebib)
                 (local-set-key (kbd "C-c i") 'ebib-insert-citation)
                 (local-set-key (kbd "C-c o") 'ebib-load-bibtex-file)
                 ;;setting up keys for texcount
                 (local-set-key (kbd "C-c c") 'latex-word-count)
                 (local-set-key (kbd "C-c w") 'latex-word-count-master)
                 ;;setting up keys for writegood mode
                 (local-set-key (kbd "C-c g") 'writegood-grade-level)
                 (local-set-key (kbd "C-c r") 'writegood-reading-ease)))))

;; nomenclature for latex
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                  (lambda (name command file)
                    (TeX-run-compile name command file)
                    (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                  nil t :help "Create nomenclature file")))

;;count words in single file
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "~/.emacs.d/packages/texcount/texcount.pl "
                         ; "uncomment then options go here "
                         (buffer-file-name))))
;;count words in entire document
(defun latex-word-count-master ()
  (interactive)
  (if (eq TeX-master t)
      (setq master (buffer-file-name))
    (setq master (concat (expand-file-name TeX-master) ".tex")))
  (shell-command (concat "~/.emacs.d/packages/texcount/texcount.pl "
                         "-dir "
                         "-unicode "
                         "-inc "
                         master)))
