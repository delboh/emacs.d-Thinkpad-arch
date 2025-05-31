;; (defun newline-after-phrase-end ()
;;   "Insert a newline after ., !, ?, ,, :, or ; if followed by more than one word.
;; The punctuation remains at the end of the current line."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward
;;             "\\([.!?,:;]\\)[ \t]+\\(\\w+\\(?:[ \t]+\\w+\\)+\\)"
;;             nil t)
;;       ;; Group 1: punctuation
;;       ;; Group 2: two or more words
;;       (replace-match (concat (match-string 1) "\n" (match-string 2)) t))))


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
		;; Group 1: punctuation
		;; Group 2: two or more words
        (replace-match (concat (match-string 1) "\n" (match-string 2)) t)))))

(defun my-sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
  Prefixed with negative \\[universal-argument], sorts in reverse.

  The variable `sort-fold-case' determines whether alphabetic case
  affects the sort order.

  See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


(defun org-forward-clause ()
  "Move point to the end of the next clause or sentence.
A clause ends with a period or comma."
  (interactive)
  (let ((pos (point)))
    (when (re-search-forward "[.,]" nil t)
      ;; Move just after the punctuation
      (forward-char 0)
      ;; Skip following whitespace
      (skip-chars-forward " \t\n"))
    ;; Return t if point moved
    (not (= pos (point)))))

;; Optionally, bind it in org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-e") #'org-forward-clause))

