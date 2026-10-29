;;; setup-learn-lang.el --- learning languages  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ethan Goan
;;
;; Author: Ethan Goan <ethan@work>
;; Maintainer: Ethan Goan <ethan@work>
;; Created: October 21, 2026
;; Modified: October 21, 2026
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/Symbol’s function definition is void: doom-call-process/setup-learn-lang
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  I want to learn some languages, particularly french :)
;;  A lot of this is coming from Sacha Chua
;; https://sachachua.com/dotemacs/index.html#multimedia-learning-french
;; https://codeberg.org/sachac/learn-lang/
;;; Code:

(use-package learn-lang
  :ensure (learn-lang
           :host codeberg
           :repo "sachac/learn-lang")
  :config
  (setq learn-lang-language "fr")
  (setq learn-lang-tatoeba-files
        '(("fr" . "~/projects/en-fr/tatoeba-fr-en-500.tsv"))))


;;;###autoload
(defun sacha-learn-lang-format-virelangues (beg end)
  "Prepare tongue twisters."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (flush-lines "Raphael\\|AM$\\|PM$\\|^$")
    (kill-new (buffer-string))
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match "- "))))

(defun sacha-learn-lang-combined-query (input)
  "Look for entries matching INPUT.
Check tatoeba, dictionary, then Google Translate."
  (or
   (seq-map
    (lambda (o)
      (cons
       (format "%s - %s" (car o) (cdr o))
       (car o)))
    (learn-lang-tatoeba-entries input))
   (seq-keep 'cdr (sacha-learn-lang-en-fr-dict-lookup input))
   (let ((translation (learn-lang-translate-text-to-target input)))
     (cons translation translation))))

;;;###autoload
(defun sacha-learn-lang-combined-consult (&optional text)
  "Search for TEXT interactively. Use Tatoeba, but fall back to dictionary or Google Translate."
  (interactive)
  (let ((val (consult--read
              (consult--dynamic-collection 'sacha-learn-lang-combined-query
                :debounce 0.5)
              :prompt "Search: "
              :lookup #'consult--lookup-cdr
              :history '(:input learn-lang-tatoeba-history)
              :add-history (list text (word-at-point))
              :initial text)))
    (when (called-interactively-p 'any)
      (insert val))
    val))

(setq search-default-mode 'char-fold-to-regexp)

(defvar-keymap sacha-learn-lang-map
  :prefix t
  "l" (cons "lookup" #'sacha-learn-lang-lexique-complete-word)
  "w" (cons "wordref" #'sacha-learn-lang-wordreference-lookup)
  "c" (cons "conj" #'sacha-learn-lang-conjugate)
  "f" (cons "→ fr" #'sacha-learn-lang-consult-en-fr)
  "s" (cons "say" #'sacha-learn-lang-say-word-at-point)
  "x" (cons "example" #'sacha-learn-lang-combined-consult)
  "t" (cons "→ en" #'sacha-learn-lang-translate-dwim))

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-," 'sacha-learn-lang-map)
  (keymap-set org-mode-map "C-c u" 'sacha-learn-lang-map))

(with-eval-after-load 'message
  (keymap-set message-mode-map "C-," 'sacha-learn-lang-map)
  )

(with-eval-after-load 'flyspell
  (keymap-set flyspell-mode-map "C-," 'sacha-learn-lang-map))

(with-eval-after-load 'agent-shell
  (keymap-set agent-shell-mode-map "C-," 'sacha-learn-lang-map))

(with-eval-after-load 'subed
  (keymap-set subed-mode-map "C-," 'sacha-learn-lang-map))


;; (use-package wiktionary-bro
;;   :config
;;   (setq wiktionary-bro-language "fr")
;;   )

(use-package flycheck-grammalecte
  :ensure t
  :config
  (setq flycheck-grammalecte-report-apos nil)
  (setq flycheck-grammalecte-report-nbsp nil)
  (setq flycheck-grammalecte-report-esp nil)
  (with-eval-after-load 'flycheck
    (flycheck-grammalecte-setup)))

;;;###autoload
(defun sacha-learn-lang-chrome-speech-new-session ()
  (interactive)
  (sacha-speech-chrome-new-session "french" "fr-FR"))

(defun sacha-learn-lang-say-repeat-with-pause (s num-times multiple filename &optional force)
  (setq s (learn-lang-subed-record-simplify s))
  (when (file-directory-p filename)
    (setq filename (expand-file-name (concat (learn-lang-slugify s) ".mp3") filename)))
  (unless (and (not force) (file-exists-p filename))
    (let ((temp-file (concat (make-temp-name "learn-lang") ".mp3"))
          duration-ms
          args)
      (learn-lang-tts-say s nil temp-file)
      (setq duration-ms (compile-media-get-file-duration-ms temp-file))
      (setq args (list "-i"
                       temp-file
                       "-filter_complex"
                       (format "sine=f=800:d=0.250,afade=t=out:st=0.150:d=0.100[chime];anullsrc=r=44100:cl=stereo:d=0.100[gap];anullsrc=r=44100:cl=stereo:d=%.3f[silence];[chime][gap][0:a][silence]concat=n=4:v=0:a=1[unit];[unit]aloop=loop=%d:size=2e9[out]"
                               (* multiple duration-ms 0.001)
                               (1- num-times))
                       "-y"
                       "-map"
                       "[out]"
                       filename))
      (with-current-buffer (get-buffer-create "*ffmpeg*")
        (insert (mapconcat #'shell-quote-argument args " ") "\n")
        (apply #'call-process learn-lang-ffmpeg-command
               nil t nil
               args))
      (delete-file temp-file)
      filename)))

;; (sacha-learn-lang-say-repeat-with-pause "Il n'y a pas une baguette magique." 3 1.5 "~/sync/music/french/orale/" t)
;;;###autoload
(defun sacha-learn-lang-say-lines-as-files (input-file num-times multiple output-directory)
  "Say each line in INPUT-FILE as an MP3 in OUTPUT-DIRECTORY."
  (interactive)
  (with-temp-buffer
    (insert-file-contents input-file)
    (dolist (s (split-string (string-trim (buffer-string)) "\n"))
      (let* ((s (learn-lang-subed-record-simplify s))
             (filename (expand-file-name (learn-lang-slugify s output-directory))))
        (sacha-learn-lang-say-repeat-with-pause s num-times multiple filename)))))


;;;###autoload
(defun sacha-learn-lang-make-tonguetwister-practice-files (vtt output-directory &optional force)
  "Parse VTT and add tonguetwister files to OUTPUT-DIRECTORY."
  (interactive "FVTT: \nDOutput directory: \nP")
  (let ((media-file (subed-guess-media-file nil vtt)))
    (dolist (sub (subed-parse-file vtt))
      (let ((filename (expand-file-name
                       (concat (learn-lang-subed-record-simplify (elt sub 3)) ".mp3")
                       output-directory))
            (compile-media-ffmpeg-arguments
             (append (list "-metadata"
                           (concat "lyrics-fra="
                                   (learn-lang-subed-record-simplify (elt sub 3))))
                     compile-media-ffmpeg-arguments))
            sources)
        (when (or force (not (file-exists-p filename)))
          (dotimes (n 10)
            (push (list :source "/home/sacha/proj/french/chime.opus")
                  sources)
            (push (list :source media-file
                        :start-ms (elt sub 1)
                        :stop-ms (elt sub 2)
                        :pad-right (* 1.5 (- (elt sub 2) (elt sub 1))))
                  sources))
          (compile-media-sync
           `((audio ,@(nreverse sources)))
           filename
           "-y"))))))

(defvar sacha-learn-lang-tonguetwister-source "~/proj/french/analysis/virelangues/"
  "Source.")
(defvar sacha-learn-lang-tonguetwister-directory "~/sync/music/french/virelangues/"
  "Output directory.")

;;;###autoload
(defun sacha-learn-lang-process-tonguetwister-clips (file)
  "Extract practice files to directory.
File should start with YYYY-MM-DD."
  (interactive (list
                (read-file-name "Clip VTT: "
                                (sacha-latest-file sacha-learn-lang-tonguetwister-source "-clips"))))
  (let* ((base-dir sacha-learn-lang-tonguetwister-directory)
         (latest (expand-file-name "latest" base-dir))
         (lyrics-dir (expand-file-name "../lyrics" base-dir)))
    (let* ((date (substring (file-name-base file) 0 10))
           (file-dir (expand-file-name (substring (file-name-base file) 0 10) base-dir)))
      (unless (file-directory-p file-dir)
        (make-directory file-dir))
      (sacha-learn-lang-make-tonguetwister-practice-files file latest)
      (sacha-learn-lang-make-tonguetwister-practice-files file file-dir)
      (dolist (sub (subed-parse-file file))
        (with-temp-file (expand-file-name (concat (elt sub 3) ".lrc")
                                          lyrics-dir)
          (insert "[00:00.00]" (elt sub 3) "\n"))))))

;;;###autoload
(defun sacha-org-collect-logbook-contents ()
  "Collect contents of all LOGBOOK drawers in the current subtree.
Returns them concatenated as a string."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t)))
          contents
          elem)
      (while (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" subtree-end t)
        (setq elem (org-element-at-point))
        (push (buffer-substring-no-properties
               (org-element-contents-begin elem)
               (org-element-contents-end elem))
              contents))
      (string-join (nreverse contents) "\n"))))

;;;###autoload
(defun sacha-org-get-subtree (link)
  (save-window-excursion
    (save-excursion
      (org-link-open-from-string link)
      (buffer-substring-no-properties (point) (progn (org-end-of-subtree) (point))))))

;;;###autoload
(defun sacha-learn-lang-gptel-analyze-feedback ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Feedback*")
    (erase-buffer)
    (org-mode))
  (gptel-request
      (json-encode
       `(("feedback on previous mistakes" . ,(sacha-org-collect-logbook-contents))
         ("topic links" . ,(sacha-org-get-subtree "[[file:~/sync/orgzly/organizer.org::#kwiziq-a2]]"))
         ("prompt" . "Analyze the feedback on previous mistakes. Map them to the different topics and create a frequency table where column A has a link to the topic and column B has the number of errors in that category. For anything that doesn't match, summarize them in a separate list called Other. Also create a 10-item quiz covering the most important points. Hide answers like this: [[answer:the answer goes here][___]] Use Org Mode syntax.")))
    :callback (lambda (response info)
                (with-current-buffer (get-buffer-create "*Feedback*")
                  (insert response)
                  (goto-char (point-min))
                  (pop-to-buffer (current-buffer))))))

;;; pronunciation
(defvar sacha-learn-lang-practice-dir "~/proj/french/audio")
(defvar sacha-learn-lang-practice-temp (expand-file-name "temp.wav" sacha-learn-lang-practice-dir))
;;;###autoload
(defun sacha-learn-lang-practice-line ()
  (interactive)
  (sit-for 1)
  (let* ((date (format-time-string "%Y-%m-%d-%H-%M-%S"))
         (filename
          (expand-file-name
           (concat date " "
                   (string-trim
                    (buffer-substring (line-beginning-position)
                                      (line-end-position)))
                   ".opus")
           sacha-learn-lang-practice-dir))
         (process (start-process "arecord"
                                 (get-buffer-create "*record*")
                                 "arecord" "-D" "hw:2,0" "-t" "wav" "-f" "cd"
                                 sacha-learn-lang-practice-temp)))
    (read-key "Press a key")
    (kill-process process)
    (call-process "ffmpeg" nil (get-buffer-create "*record*") nil
                  "-y"
                  "-i"
                  sacha-learn-lang-practice-temp
                  "-af" "silenceremove=start_periods=1:start_duration=0:start_threshold=-60dB,areverse,silenceremove=start_periods=1:start_duration=0:start_threshold=-60dB,areverse,loudnorm=I=-16:LRA=11:TP=-1.5"
                  filename)
    (mpv-play filename)))

;;;###autoload
(defun sacha-learn-lang-practice-replay ()
  (interactive)
  (mpv-play (sacha-latest-file sacha-learn-lang-practice-dir)))

;;;###autoload
(defun sacha-learn-lang-practice-transcribe ()
  (interactive)
  (let* ((default-directory sacha-learn-lang-practice-dir)
         (file (sacha-latest-file sacha-learn-lang-practice-dir "\\.opus\\|\\.m4a\\|\\.webm")))
    ;; (call-process "/bin/bash" nil nil nil "/home/sacha/bin/whisperx" (expand-file-name file))
    (message "%s"
             (with-temp-buffer
               (insert-file-contents (concat (file-name-sans-extension file) ".txt"))
               (string-trim (buffer-string))))))

;;;###autoload
(defun sacha-learn-lang-practice-play-current-reference (&optional beg end)
  "Play the current segment."
  (interactive)
  (when (derived-mode-p 'subed-mode)
    (let ((comment (subed-subtitle-comment)))
      (cond
       ((string-match "#\\+REFERENCE: \\(.+\\) +\\(.+?\\) +--> +\\([^ ]+\\)" comment)
        (let ((file (expand-file-name (match-string 1 comment)))
              (start (match-string 2 comment))
              (stop (match-string 3 comment)))
          (call-process "mpv" nil nil nil
                        file
                        (format "--start=%.3f" (/ (subed-timestamp-to-msecs start) 1000.0))
                        (format "--end=%.3f" (/ (subed-timestamp-to-msecs stop) 1000.0)))))
       ((string-match "#\\+REFERENCE: \\(.+\\)" comment)
        (call-process "mpv" nil nil nil (match-string 1 comment)))
       (t (let ((start
                 (if beg (save-excursion
                           (goto-char beg)
                           (subed-subtitle-msecs-start))
                   (subed-subtitle-msecs-start)))
                (stop
                 (if beg (save-excursion
                           (goto-char end)
                           (subed-subtitle-msecs-stop))
                   (subed-subtitle-msecs-stop))))
            (call-process "mpv" nil nil nil (subed-media-file)
                          (format "--start=%.3f" (/ start 1000.0))
                          (format "--end=%.3f" (/ stop 1000.0)))))))))

;;;###autoload
(defun sacha-learn-lang-practice-record-loop (&optional extra)
  (interactive
   (list (cond
          ((and (derived-mode-p 'subed-mode)
                (region-active-p))
           (subed-subtitle-list-text
            (subed-subtitle-list
             (region-beginning)
             (region-end))))
          ((region-active-p)
           (concat
            " "
            (string-trim
             (buffer-substring (region-beginning) (region-end)))))
          ((derived-mode-p 'subed-mode)
           (concat " " (subed-subtitle-text))))))
  (let* ((date (format-time-string "%Y-%m-%d-%H-%M-%S"))
         (key-prompt "%d (SPC to review, RET get feedback, q to quit, any other key to retry")
         (filename
          (expand-file-name
           (concat date
                   (if extra
                       (concat " "
                               (string-trim (replace-regexp-in-string
                                             "[\\?]+" " "
                                             (car
                                              (split-string
                                               extra "\n")))))
                     "")

                   ".opus")
           sacha-learn-lang-practice-dir))
         done
         char
         (count 0)
         process)
    (while (not done)
      (when (derived-mode-p 'subed-mode)
        (if (region-active-p)
            (sacha-learn-lang-practice-play-current-reference (region-beginning) (region-end))
          (sacha-learn-lang-practice-play-current-reference)))
      (setq count (1+ count))
      (setq process (start-process "ffmpeg"
                                   (get-buffer-create "*record*")
                                   "ffmpeg" "-y" "-f" "pulse" "-i" "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo"
                                   sacha-learn-lang-practice-temp))
      (setq char
            (read-key (format key-prompt
                              count)))
      (while char
        (pcase char
          (?\  (when (process-live-p process)
                 (sit-for 1)
                 (kill-process process))
               (sacha-learn-lang-practice-play-current-reference)
               (call-process "mpv" nil nil nil sacha-learn-lang-practice-temp)
               (setq char
                     (read-key (format "%d (SPC to review, RET get feedback, q to quit, any other key to retry"
                                       count))))
          (13 (when (process-live-p process)
                (sit-for 1)
                (kill-process process))
              (setq done 'feedback)
              (setq char nil))
          (?w            ; transcribe with Whisper
           (sacha-learn-lang-practice-transcribe))
          (?q
           (when (process-live-p process) (kill-process process))
           (setq done 'ignore)
           (setq char nil))
          (_
           (when (process-live-p process) (kill-process process))
           (setq char nil)))))
    (when (member done '(keep feedback))
      (call-process "ffmpeg" nil (get-buffer-create "*record*") nil
                    "-y"
                    "-i"
                    sacha-learn-lang-practice-temp
                    "-af" "loudnorm=I=-16:LRA=11:TP=-1.5"
                    ;; "-af" "silenceremove=start_periods=1:start_duration=0:start_threshold=-100dB,areverse,silenceremove=start_periods=1:start_duration=0:start_threshold=-100dB,areverse,loudnorm=I=-16:LRA=11:TP=-1.5"
                    filename)
      (when (eq done 'feedback)
        (sacha-learn-lang-get-audio-feedback filename extra t)))))


(require 'url)
(require 'json)

(defun my/french-word-of-the-day ()
  "Fetch FrenchDictionary.com's word of the day."
  (with-current-buffer
      (url-retrieve-synchronously
       "https://www.frenchdictionary.com/wordoftheday"
       t t)
    (goto-char (point-min))
    (re-search-forward "^$")

    (let ((html (buffer-substring-no-properties
                 (point)
                 (point-max))))

      (unless
          (string-match
           "window\\.SD_COMPONENT_DATA = \\({.*?}\\);"
           html)
        (error "Could not find SD_COMPONENT_DATA"))

      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (data (json-parse-string
                    (match-string 1 html)
                    :object-type 'alist
                    :array-type 'list))
             (today (car (alist-get 'wotdList data))))

        `((word . ,(alist-get 'word today))
          (translation . ,(alist-get 'translation today))
          (example-fr . ,(alist-get 'exampleSource today))
          (example-en . ,(alist-get 'exampleTranslated today))
          (pronunciation . ,(alist-get 'paDisplay today))
          (part-of-speech . ,(alist-get 'pos today))
          (published . ,(alist-get 'publishedAt today)))))))


(setq french-word-of-day-alist (my/french-word-of-the-day))

(provide 'setup-learn-lang)
;;; setup-learn-lang.el ends here
