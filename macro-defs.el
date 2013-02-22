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

;; rotate two buffers
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	  (b1 (window-buffer w1))
	   (b2 (window-buffer w2))
	    (s1 (window-start w1))
	     (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

(provide 'macro-defs)
