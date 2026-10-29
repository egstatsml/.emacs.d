;;; setup-anki.el -*- lexical-binding: t; no-byte-compile: t; -*-



(use-package sqlite3
  :ensure (:host github :repo "pekingduck/emacs-sqlite3-api"))

(use-package anki-editor
  :ensure (:host github :repo "anki-editor/anki-editor"))

(use-package ankiorg
  :ensure (:host github :repo "orgtre/ankiorg")
  :custom
  (ankiorg-sql-database
   "/home/ethan/.local/share/Anki2/User 1/collection.anki2")
  (ankiorg-media-directory
   "/home/ethan/.local/share/Anki2/User 1/collection.media"))

(provide 'setup-anki)
