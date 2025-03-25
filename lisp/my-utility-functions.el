
(defun my-end-of-line-and-newline ()
  "Move to the end of the line and insert a newline."
  (interactive)
  (end-of-line)
  (newline))

(defun my-clean-empty-lines ()
  "Remove blank lines.
Works on whole buffer or text selection, respects `narrow-to-region'.
URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22 2020-09-08"
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

;; from https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
(defun my-comment-delete (arg)
  "Delete the first comment on this line, if any.  Don't touch
the kill ring.  With prefix ARG, delete comments on that many
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

(defun my-move-to-paragraph-end ()
  "Move point to the end of the current paragraph."
  (interactive)
  (forward-paragraph)
  (backward-char))

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

;; Save the buffer (or region) to the `kill-ring' after stripping extra whitespace and new lines
;; Adapted From https://gist.github.com/xahlee/d364cbbff9b3abd12d29
(defun my-copy-simple (&optional beg end)
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

(defun my-slugify (start end)
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
