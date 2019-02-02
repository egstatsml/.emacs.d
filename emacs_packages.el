
;;Istalled packages and script to install them
;;
;;Can get a list of installed packages from Emacs
;;by running C-h v package-activated-list
;;Can then copy and format the list of installed packages using
;;sed or something similar to put it in the Elisp format
;;Then run this buffer and should be good to go

;; setting package archives and priorities
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("org" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))


;; function that will install all the packages
(defun require-package (package)
    "Install given PACKAGE. This is from Bling's config"
    (unless (package-installed-p package)
        (unless (assoc package package-archive-contents)
        (package-refresh-contents))
    (package-install package)))

;; list of packages to install
(setq pkgs '(ac-html
dash
f
dash
s
s
auto-complete
popup
auto-complete-auctex
auto-complete
popup
yasnippet
company-auctex
auctex
company
yasnippet
ebib
parsebib
seq
dash
ess-R-data-view
ess
julia-mode
popup
ctable
ess-smart-equals
ess
julia-mode
ess-smart-underscore
ess
julia-mode
ess-view
f
dash
s
s
ess
julia-mode
excorporate
nadvice
url-http-ntlm
ntlm
soap-client
fsm
fsm
helm-bibtex
biblio
biblio-core
dash
seq
let-alist
f
dash
s
dash
s
parsebib
helm
helm-core
async
popup
async
julia-mode
langtool
magit
with-editor
async
magit-popup
dash
async
let-alist
git-commit
with-editor
async
dash
ghub
let-alist
dash
async
magit-popup
dash
async
markdown-mode
nadvice
helm-core
async
parsebib
popup
s
seq
soap-client
stan-mode
url-http-ntlm
ntlm
with-editor
async
writegood-mode
yasnippet)) ;; (you probably want to format the packages.txt first, otherwise you get a huge single line)
(require 'cl) ;;g Common Lisp compatibility library
(loop for pkg in pkgs do
  (require-package pkg)) ;; install each package

;; install org-wiki
(let ((url "https://raw.githubusercontent.com/caiorss/org-wiki/master/org-wiki.el"))     
      (with-current-buffer (url-retrieve-synchronously url)
	(goto-char (point-min))
	(re-search-forward "^$")
	(delete-region (point) (point-min))
	(kill-whole-line)
	(package-install-from-buffer)))
