;;settings for R
(require 'ess-view)
;;view dataframe with C-x w

;;change assign shortkey to semi-colon
(add-hook  'R-mode-hook 'r-smart-mode)
(defun r-smart-mode ()
  (local-set-key (kbd ";")  (lambda () (interactive) (insert " <- ")))
  (ess-toggle-underscore nil))

(setq ess-default-style 'RStudio)
;; For STAN
(require 'stan-mode)
