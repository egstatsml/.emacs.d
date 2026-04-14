;;; setup-term.el -*- lexical-binding: t; no-byte-compile: t; -*-


;; graphics in the term
(use-package kitty-graphics
  :ensure (kitty-graphics
           :host github
           :repo "cashmeredev/kitty-graphics.el")
  :if (not (display-graphic-p))
  :config
  (kitty-graphics-mode 1))

(provide 'setup-term)
