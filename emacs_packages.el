;;Installed packages and script to install them
;;
;;Can get a list of installed packages from Emacs
;;by running C-h v package-activated-list
;;Can then copy and format the list of installed packages using
;;sed or something similar to put it in the Elisp format
;;Then run this buffer and should be good to go
;;
;;
;; Can use "C-q C-j" to insert newline special char to replace
;; spaces (obtained when using package-activated-list) with
;; a newline
;;
;; setting package archives and priorities
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
        '(("org" . 20)
          ("gnu" . 10)
          ("melpa" . 100)))


;; function that will install all the packages
(defun require-package (package)
    "Install given PACKAGE. This is from Bling's config"
    (unless (package-installed-p package)
        (unless (assoc package package-archive-contents)
        (package-refresh-contents))
    (package-install package)))

;; list of packages to install
(setq pkgs
      '(0blayout
        ac-html
all-the-icons-ivy
all-the-icons-ivy-rich
apropospriate-theme
auctex-latexmk
auctex
auto-complete-auctex
auto-complete
calfw
color-theme
color-theme-sanityinc-solarized
color-theme-sanityinc-tomorrow
counsel-projectile
counsel
dap-mode
bui
doom-modeline
doom-themes
all-the-icons
ebib
ein
deferred
anaphora
elpy
company
ess-R-data-view
ctable
ess-smart-equals
ess-smart-underscore
ess-view
ess
evil-collection
annalist
evil
excorporate
flycheck
forge
closql
fsm
ghub
goto-chg
helm-ls-git
helm-org
helpful
elisp-refs
highlight-indentation
ivy-bibtex
ivy-prescient
ivy-rich
jupyter
langtool
languagetool
loop
lsp-ivy
lsp-python-ms
lsp-treemacs
lsp-mode
dash-functional
magit
git-commit
markdown-mode
matlab-mode
mu4e-alert
alert
log4e
gntp
org-bullets
org-journal
org-kanban
org-noter
org-roam-bibtex
org-ref
key-chord
helm-bibtex
helm
htmlize
bibtex-completion
biblio
biblio-core
org-roam
magit-section
emacsql-sqlite
emacsql
org-super-agenda
helm-core
parsebib
pdf-view-restore
pdf-tools
pkg-info
epl
polymode
popup
posframe
prescient
projectile
pyvenv
rainbow-delimiters
request
shrink-path
simple-httpd
soap-client
spinner
stan-mode
swiper
ivy
tablist
transient
treemacs
ht
hydra
lv
pfuture
ace-window
avy
f
treepy
ts
s
dash
undo-tree
url-http-ntlm
use-package
bind-key
wakatime-mode
web-completion-data
websocket
wgrep
which-key
with-editor
writegood-mode
yaml
yasnippet
zenburn-theme
zmq)) ;; (you probably want to format the packages.txt first, otherwise you get a huge single line)
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
