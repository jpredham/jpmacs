;;; python-defs.el

(defun macro/python-check ()
  "Runs pyflakes and pep8 on current file"
  (interactive)
  (let ((path (file-name-nondirectory buffer-file-name)))
    (compile (format "pyflakes %s ; pep8 --repeat %s" path path))))


(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-c c") 'macro/python-check)
     (define-key python-mode-map (kbd "C-c l") "lambda")
     (define-key python-mode-map (kbd "M-/") 'hippie-expand)
     (add-hook 'python-mode-hook 'macro/run-coding-hook)))

(provide 'python-defs)
