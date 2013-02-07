;;; jp-python.el

(defun jp/python-check ()
  "Runs pyflakes and pep8 on current file"
  (interactive)
  (let ((path (file-name-nondirectory buffer-file-name)))
    (compile (format "pyflakes %s ; pep8 --repeat %s" path path))))


(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
  they line up with the line containing the corresponding opening bracket."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")
               (looking-at "\\s)"))
          (progn
            (forward-char 1)
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))
        ad-do-it))))

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-c c") 'jp/python-check)
     (define-key python-mode-map (kbd "C-c l") "lambda")
     (define-key python-mode-map (kbd "M-/") 'hippie-expand)
     (ad-activate 'python-calculate-indentation)
     (add-hook 'python-mode-hook 'jp/run-coding-hook)))

(provide 'jp-python)
