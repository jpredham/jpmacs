;;; init.el

;; Package System and Load Path
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")
(package-initialize)

;; Core Packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar core-packages '(org haskell-mode autopair python-mode pymacs jinja2-mode pretty-mode auto-complete ruby-mode haml-mode yaml-mode)
  "Ensure these packages are installed at launch.")

(dolist (p core-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Custom Key Bindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "M-g g") 'goto-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x t") 'transpose-lines)
(global-set-key (kbd "C-x C-t") 'other-window)
(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)

;; Emacs config
(require 'saveplace)
(require 'ffap)
(require 'ansi-color)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default indent-line-function 'insert-tab)
(setq-default c-basic-offset 4)
(setq-default c-file-style nil)
(setq-default fill-column 78)
(setq-default truncate-lines t)
(setq-default css-indent-offset 2)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)
(if (fboundp 'x-cut-buffer-or-selection-value)
    (setq x-select-enable-clipboard t
          interprogram-paste-function 'x-cut-buffer-or-selection-value))
(defalias 'yes-or-no-p 'y-or-n-p)
(random t)
(delete 'try-expand-line hippie-expand-try-functions-list)
(show-paren-mode 1)
(global-font-lock-mode t)
(auto-compression-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode t)
(global-linum-mode 1)
(setq column-number-mode t)
(setq-default save-place t)

;; Custom Libraries
(require 'macro-defs)
(require 'python-defs)
(add-hook 'macro/coding-hook 'macro/highlight_longlines)

;;Uniquify
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator " : ")

;;Auto-complete Section:
(require 'auto-complete)
(global-auto-complete-mode)

;; Python
(require 'python)
(require 'pymacs)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 2
                  tab-always-indent t
                  indent-tabs-mode  nil
                  python-indent 2)))

(setq py-load-pymacs-p nil)

;;Haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;Org-mode
(require 'org)
(require 'ob)
(require 'ob-tangle)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (python . t)
   (java . t)
   (haskell . t)
   (sh . t)
   (gnuplot . t)))

;;Pretty-mode
(require 'pretty-mode)
(global-pretty-mode 1)

;; Jinja
(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

;; ESS
;(require 'ess-site)
;(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
;(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session

(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))


;; Ruby + Rails
(require 'ruby-mode)
(require 'haml-mode)
;(require 'scss-mode)
;(setq scss-compile-at-save nil)
;(require 'sass-mode)


;;Autopair
(require 'autopair)
(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)


;; Javascript
(setq js-indent-level 2)


;; Yaml
(require 'yaml-mode)
