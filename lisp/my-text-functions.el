(defun newline-after-phrase-end ()
  "Insert a newline after ., !, ?, ,, :, or ; if followed by more than one word.
The punctuation remains at the end of the current line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\([.!?,:;]\\)[ \t]+\\(\\w+\\(?:[ \t]+\\w+\\)+\\)"
            nil t)
      ;; Group 1: punctuation
      ;; Group 2: two or more words
      (replace-match (concat (match-string 1) "\n" (match-string 2)) t))))


(defun my-newline-after-phrase-end (start end)
  "Reformat prose into lines by inserting a newline after punctuation.
Operates on the active region if selected, or the whole buffer otherwise.
Punctuation remains at the end of the current line."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Narrow to region if it's active
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward
              "\\([.!?,:;]\\)[ \t]+\\(\\w+\\(?:[ \t]+\\w+\\)+\\)"
              nil t)
        (replace-match (concat (match-string 1) "\n" (match-string 2)) t)))))
      
