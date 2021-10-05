;;;spell checking
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-<f8>") 'flyspell-buffer)
(global-set-key (kbd "<f10>") 'flyspell-buffer)
(global-set-key (kbd "<f7>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "<f9>") 'flyspell-check-next-highlighted-word)
(setq ispell-dictionary "british")    ;set the default dictionary
;;Go to next mispelt word
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (when run-together
        (cond
         ;; Kevin Atkinson said now aspell supports camel case directly
         ;; https://github.com/redguardtoo/emacs.d/issues/796
         ((string-match-p "--camel-case"
                          (shell-command-to-string (concat ispell-program-name " --help")))
          (setq args (append args '("--camel-case"))))

         ;; old aspell uses "--run-together". Please note we are not dependent on this option
         ;; to check camel case word. wucuo is the final solution. This aspell options is just
         ;; some extra check to speed up the whole process.
         (t
          (setq args (append args '("--run-together" "--run-together-limit=16")))))))

     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary' itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup `ispell-local-dictionary-alist'
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 (t (setq ispell-program-name nil)))

;; `ispell-cmd-args' is useless, it's the list of *extra* arguments we will append to the ispell process when `ispell-word' is called.
;; `ispell-extra-args' is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, `ispell-extra-args' will NOT be used.
;; Hack `ispell-local-dictionary-alist' instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))

(defun my-ispell-word-hack (orig-func &rest args)
  "Use Emacs original arguments when calling `ispell-word'.
When fixing a typo, avoid pass camel case option to cli program."
  (let* ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original argument
    (setq ispell-extra-args (my-detect-ispell-args))
    (apply orig-func args)
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(advice-add 'ispell-word :around #'my-ispell-word-hack)
(advice-add 'flyspell-auto-correct-word :around #'my-ispell-word-hack)

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)
;; setting up for prog mode spell checking
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; add spell checking for git commit messages
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
