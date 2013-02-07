;;; jp-defuns.el

(defun jp/run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'jp/coding-hook))


;; prettify lambda keyword
(defun jp/pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


(defun jp/highlight_longlines ()
  (font-lock-add-keywords 
     nil
     '(("^[^\n]\\{80\\}\\(.*\\)$"
     1 font-lock-warning-face prepend))))


;; kill the characters from the cursor to the beginning of the line
(defun jp/backward-kill-line ()
  "Kill chars backward until encountering the end of a line."
  (interactive)
  (kill-line 0))

(provide 'jp-defuns)
