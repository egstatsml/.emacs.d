;; Use IPython for REPL
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-d") 'my-docstring-column)))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
