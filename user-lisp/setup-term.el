;;; setup-term.el -*- lexical-binding: t; no-byte-compile: t; -*-


;; graphics in the term
(use-package kitty-graphics
  :ensure (kitty-graphics
           :host github
           :repo "cashmeredev/kitty-graphics.el")
  :if (not (display-graphic-p))
  :config
  (kitty-graphics-mode 1))


(use-package ghostel
  :ensure t)

(use-package hel-ghostel
  :ensure (hel-ghostel
           :host github
           :repo "anuvyklack/hel-ghostel")
  :after (ghostel hel)
  :hook (ghostel-mode . hel-ghostel-mode))


;; when in emacs -nw, will make the cursor act how I want
(use-package term-cursor
  :ensure (term-cursor
           :host github
           :repo "h0d/term-cursor.el")
  :config
  (when (not my/graphical)
    (global-term-cursor-mode)))

(provide 'setup-term)
