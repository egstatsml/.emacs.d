;;Istalled packages and script to install them
;;
;;Can get a list of installed packages from Emacs
;;by running C-h v package-activated-list
;;Can then copy and format the list of installed packages using
;;sed or something similar to put it in the Elisp format
;;Then run this buffer and should be good to go


(defun require-package (package)
    "Install given PACKAGE. This is from Bling's config"
    (unless (package-installed-p package)
        (unless (assoc package package-archive-contents)
        (package-refresh-contents))
    (package-install package)))

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
org-wiki
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
