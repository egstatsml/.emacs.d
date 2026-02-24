;;; helheim-window.el -*- lexical-binding: t; no-byte-compile: t; -*-
;; Eagarly initialise hydra
(use-package hydra
  :ensure t)


(use-package ace-window
  :ensure t)

;; * my window hydra
(use-package my/window-hydra
  :no-require t
  :after (hydra)
  :config
  ;; load in windmove
  (require 'windmove)
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'right))
	(shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'right))
	(enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'up))
	(enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'up))
	(shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-window ()
            "
  Movement^^        ^Split^         ^Switch^		^Resize^
  ----------------------------------------------------------------
  _h_ ←		_v_ertical	_b_uffer		_q_ X←
  _j_ ↓		_x_ horizontal	_f_ind files	_w_ X↓
  _k_ ↑		_z_ undo	_a_ce 1		_e_ X↑
  _l_ →		_Z_ reset	_s_wap		_r_ X→
  _F_ollow		_D_lt Other	_S_ave		max_i_mize
  _SPC_ cancel	_o_nly this	_d_elete
  "
            ("h" windmove-left)
            ("j" windmove-down)
            ("k" windmove-up)
            ("l" windmove-right)
            ("q" hydra-move-splitter-left)
            ("w" hydra-move-splitter-down)
            ("e" enlarge-window)
            ("r" hydra-move-splitter-right)
            ("b" consult-buffer)
            ("f" find-file)
            ("F" follow-mode)
            ("a" (lambda ()
	           (interactive)
	           (ace-window 1)
	           (add-hook 'ace-window-end-once-hook
		             'hydra-window/body)))
            ("v" (lambda ()
	           (interactive)
	           (split-window-right)
	           (windmove-right)))
            ("x" (lambda ()
	           (interactive)
	           (split-window-below)
	           (windmove-down)))
            ("s" (lambda ()
	           (interactive)
	           (ace-window 4)
	           (add-hook 'ace-window-end-once-hook
		             'hydra-window/body)))
            ("S" save-buffer)
            ("d" delete-window)
            ("D" (lambda ()
	           (interactive)
	           (ace-window 16)
	           (add-hook 'ace-window-end-once-hook
		             'hydra-window/body)))
            ("o" delete-other-windows)
            ("i" ace-maximize-window)
            ("z" (progn
	           (winner-undo)
	           (setq this-command 'winner-undo)))
            ("Z" winner-redo)
            ("SPC" nil)
            ;; resizing
            ("q" (lambda ()
	           (interactive)
	           (enlarge-window-horizontally 10)))
            ("r" (lambda ()
	           (interactive)
	           (shrink-window-horizontally 10))))


  ;;; my window management
  ;; taken from https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  (defun ethan/split-below (arg)
    "Split window below from the parent or from root with ARG."
    (interactive "P")
    (split-window (if arg (frame-root-window)
		    (window-parent (selected-window)))
		  nil 'below nil))

  (defun ethan/toggle-window-dedication ()
    "Toggle window dedication in the selected window."
    (interactive)
    (set-window-dedicated-p (selected-window)
			    (not (window-dedicated-p (selected-window))))))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Embark\\*"
          platformio-compilation-mode
          help-mode
          helpful-mode))
  ;; compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints


(use-package winpulse
  :ensure (winpulse
           :host github
           :repo "xenodium/winpulse")
  :config
  (winpulse-mode +1))

(provide 'helheim-window)
