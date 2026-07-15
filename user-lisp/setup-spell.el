;;; setup-spell.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook hook #'jinx-mode))
  (with-eval-after-load 'vertico
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
    (vertico-multiform-mode)))

(provide 'setup-spell)
