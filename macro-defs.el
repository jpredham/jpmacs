;;; macro-defs.el

(defun macro/run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'macro/coding-hook))


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
