;;; macro-defs.el

(defun macro/run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'macro/coding-hook))


;; prettify lambda keyword
(defun macro/pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


(defun macro/highlight_longlines ()
  (font-lock-add-keywords 
     nil
     '(("^[^\n]\\{80\\}\\(.*\\)$"
     1 font-lock-warning-face prepend))))


;; kill the characters from the cursor to the beginning of the line
(defun macro/backward-kill-line ()
  "Kill chars backward until encountering the end of a line."
  (interactive)
  (kill-line 0))

(provide 'macro-defs)
