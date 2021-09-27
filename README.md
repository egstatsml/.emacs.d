My Emacs configuration that I use on most systems.
Slight alterations may need to be made for running on some systems (such as high performance clusters without a display environment)

Where possible, I have tried to list other peoples code as a submodle.

For those that arent possible (because I could not find them on Git)
or those from the Emacs Wiki, the links are provided as:

    pp-c-l.el: Emacs Wiki https://www.emacswiki.org/emacs/PrettyControlL
    column-marker.el: Emacs Wiki https://www.emacswiki.org/emacs/ColumnMarker
    matlab-mode: Matlab mode for Emacs https://sourceforge.net/p/matlab-emacs/src/ci/master/tree/
    texcount.pl: Perl script for counting text in LaTeX http://app.uio.no/ifi/texcount/download.html

# Pulling submodules
`git submodule update --init --recursive`

# Installing Packages Through MELPA etc.
The list of installed packages through the package manager are listed in [emacs_packages.el](./emacs_packages.el). Open this script and execute the buffer to install packages.

If you want to get an updated list of the installed packages, run `C-h v activated-packages-list` to see which packages you have installed. Can then copy them and use the `tr` command in `bash` to format them (need to replace spaces with a new line so Elisp can handle it). Eg.

```bash
echo <paste contents of C-h v activated-packages-list here> | tr " " "\n"
```


## Updating packages
1. run `M-x package-list-packages`
2. hit `U` (note is capital) to mark packages that can be updated
3. hit `x` to perform update
