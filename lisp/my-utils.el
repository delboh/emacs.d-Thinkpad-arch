;;; my-utils.el --- Helpful utility functions -*- lexical-binding: t; -*-

;; Author: Your Name <delboh@hotmail.com>
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/yourname/my-utils

;;; Commentary:

;; This file provides a set of small utility functions that help with
;; buffer management, navigation, and common editing tasks.
;;
;; Load this file with:
;;   (require 'my-utils)
;;
;; See individual function docstrings for usage details.

;;; Code:

(defun my-move-to-paragraph-end ()
  "Move point to the end of the current paragraph."
  (interactive)
  (forward-paragraph)
  (backward-char))

(defun my-end-of-line-and-newline ()
  "Move to the end of the line and insert a newline."
  (interactive)
  (end-of-line)
  (newline))


;; URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
;; Version 2017-09-22 2020-09-08
(defun my-clean-empty-lines ()
  "Remove blank lines from whole buffer or text selection, respects 'narrow-to-region'."
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n+" nil "move")
            (replace-match "\n")))))))

;;; UNFILL REGION / PARAGRAPH

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
      logical line.  This is useful, e.g., for use with
      `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


;;; DELETE COMMENTS

;; from https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
(defun my-comment-delete (arg)
  "Delete all comnents from buffer without adding to kill ring.
With prefix ARG delete comments on that many
lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
		(when cs
		  (goto-char cs)
		  (skip-syntax-backward " ")
		  (setq cs (point))
		  (comment-forward)
		  ;; (kill-region cs (if (bolp) (1- (point)) (point))) ; original
		  (delete-region cs (if (bolp) (1- (point)) (point)))  ; replace kill-region with delete-region
		  (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun my-comment-delete-all (beg end arg)
  "Delete comments without touching the kill ring.  With active
region, delete comments in region.  With prefix, delete comments
in whole buffer.  With neither, delete comments on current line."
  (interactive "r\nP")
  (let ((lines (cond (arg
                      (count-lines (point-min) (point-max)))
                     ((region-active-p)
                      (count-lines beg end)))))
    (save-excursion
      (when lines
        (goto-char (if arg (point-min) beg)))
      (comment-delete (or lines 1)))))


;;; SLUGIFY FUNCTIONS

(defun my-slugify (start end)
  "Slugify with underscores"
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (save-excursion
          (delete-region start end)
          (insert
           (replace-regexp-in-string
            "[^a-z0-9_]" ""
            (replace-regexp-in-string
             "\s+" "_"
             (downcase regionp)
             )))))))

(defun my-slugify-web (start end)
  "Slugify with hyphens"
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (save-excursion
          (delete-region start end)
          (insert
           (replace-regexp-in-string
            "[^a-z0-9-]" ""
            (replace-regexp-in-string
             "\s+" "-"
             (downcase regionp)
             )))))))


;;; BUFFER UTILITES

(defun my-move-to-minibuffer ()
  "Move point to the active minibuffer if it exists."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; from https://gist.github.com/hyOzd/23b87e96d43bca0f0b52
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun my-delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; Save the buffer (or region) to the `kill-ring' after stripping extra whitespace and new lines
;; Adapted From https://gist.github.com/xahlee/d364cbbff9b3abd12d29
(defun my-copy-clean (&optional beg end)
  "Save the current region (or whole buffer) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer 
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))


;;; MARKDOWN TO ORG FUNCTIONS (USING PANDOC)

(defun my-md-to-org-region (start end)
  "Convert markdown formatted text in region (START, END) to org using pandoc."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))

(defun my-md-to-org ()
  "Convert Markdown file to Org file using pandoc, prompting for input and output files, then open the new Org file."
  (interactive)
  (let* ((input-file (expand-file-name (read-file-name "Input Markdown file: " nil nil t)))
         (output-file (expand-file-name (read-file-name "Output Org file: " nil nil nil
                                                        (concat (file-name-base input-file) ".org"))))
         (command (format "pandoc -f markdown -t org --lua-filter=/home/ebo/.pandoc/filters/remove-header-attr.lua --wrap=preserve %s -o  %s"
                          (shell-quote-argument input-file)
                          (shell-quote-argument output-file)))
         (result (shell-command-to-string command)))
    (if (file-exists-p output-file)
        (progn
          (find-file output-file)
          (message "Conversion complete: %s -> %s" input-file output-file))
      (message "Conversion failed. Command output: %s" result))))


;;; MY LOOKUP FUNCTIONS

(defun my-lookup-googlescholar ()
  "Look up the word under cursor in google scholar.
  If there is a text selection (a phrase), use that.
  This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
	;;      (browse-url (concat "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q="word"&btnG="))
    (eww (concat "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q="word"&btnG="))
    ))


(defun my-lookup-oed ()
  "Look up the word under cursor in oed.
  If there is a text selection (a phrase), use that.
  This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url-generic (concat "https://www.oed.com/search?searchType=dictionary&q="word"&_searchBtn=Search"))
	;;      (eww (concat "https://www.oed.com/search?searchType=dictionary&q="word"&_searchBtn=Search"))
    ))

(defun my-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
  If there is a text selection (a phrase), use that.
  Opens in eww."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
	;;      (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    (eww (concat "http://en.wikipedia.org/wiki/" word))
    ))

;;; ORG WORD COUNT
(with-eval-after-load 'org
  (defun my-org-count-words ()
	"Count words in the current Org buffer, excluding headings, properties drawers, block comments, captions, and bibliography."
	(interactive)
	(let ((word-count 0)
          (in-drawer nil)
          (in-comment-block nil))
      (save-excursion
		(goto-char (point-min))
		(while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
			(cond
			 ;; Detect start of property drawer
			 ((string-match-p "^:PROPERTIES:$" line)
              (setq in-drawer t))
			 ;; Detect end of property drawer
			 ((and in-drawer (string-match-p "^:END:$" line))
              (setq in-drawer nil))
			 ;; Detect start of block comment
			 ((string-match-p "^#\\+BEGIN_COMMENT" line)
              (setq in-comment-block t))
			 ;; Detect end of block comment
			 ((and in-comment-block (string-match-p "^#\\+END_COMMENT" line))
              (setq in-comment-block nil))
			 ;; Ignore headings
			 ((string-match-p "^\\*+ " line))
			 ;; Ignore captions
			 ((string-match-p "^#\\+CAPTION:" line))
			 ((string-match-p "^#\\+caption:" line))
			 ;; Ignore bibliography
			 ((string-match-p "^#\\+print_bibliography" line))
			 ;; Ignore inline comments
			 ((string-match-p "^# " line))
			 ;; Process normal lines if not inside a drawer or comment block
			 ((not (or in-drawer in-comment-block))
              (setq word-count (+ word-count
                                  (length (split-string line "\\W+" t)))))))
          (forward-line 1)))
      (message "Word count: %d" word-count))))


;; org export fix from chatgpt
(defun my/org-latex--move-label-before-heading (orig-fun &rest args)
  (let* ((headline (car args))
         (contents (cadr args))
         (result (apply orig-fun args)))
    ;; Match \label after a heading like \chapter{...} or \section{...}
    (if (string-match "\\(\\\\\\(chapter\\|section\\|subsection\\|subsubsection\\|paragraph\\|subparagraph\\){.*?}\\)[ \n]*\\\\label{\\([^}]+\\)}" result)
        (replace-match "\\\\label{\\3}\n\\1" nil nil result)
      result)))

(advice-add 'org-latex--format-headline-default-function :around
            #'my/org-latex--move-label-before-heading)

;; comment-dwim replacement
(defun my/org-comment-region-or-line ()
  "Comment or uncomment the current line or region in org-mode, predictably."
  (interactive)
  (let ((comment-style 'plain)) ;; avoid alignment guessing
    (if (use-region-p)
        ;; Adjust region to start and end at line boundaries
        (let ((beg (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
              (end (save-excursion (goto-char (region-end)) (if (bolp) (point) (line-end-position)))))
          (comment-or-uncomment-region beg end))
      ;; Single line
      (org-comment-line 1))))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-;") #'my/org-comment-region-or-line)))

;; Cut to gpt
(defun gpt-copy-region-and-open ()
  "Copy region and open ChatGPT in browser."
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end))
    (browse-url-chromium "https://chat.openai.com")))

(defun gptel-send-region-replace (prompt)
  "Send region to GPT with PROMPT and replace it with the response."
  (interactive "sPrompt to GPT: ")
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gptel-request
         text
         :context prompt
         :callback (lambda (response)
                     (save-excursion
                       (goto-char start)
                       (delete-region start end)
                       (insert response)))))
    (message "No region selected.")))


;; DEPENDENCY??

(defun insert-todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))



(provide 'my-utils)
;;; my-utils.el ends here
