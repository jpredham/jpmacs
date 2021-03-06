;;; init.el


;; my elisp
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "keys")
(load-library "funcs")


;; Enables basic packaging support
(require 'package)

;; Package System and Load Path
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
  ))

(add-to-list 'load-path "~/.emacs.d/elpa")
(package-initialize)

;; Core Packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar core-packages '(org autopair elpy flycheck blacken yaml-mode)
  "Ensure these packages are installed at launch.")

(dolist (p core-packages)
  (when (not (package-installed-p p))
    (package-install p)))


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


;;Uniquify
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator " : ")

;; Python
(require 'python)
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;Autopair
(require 'autopair)
(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)

;; Javascript
(setq js-indent-level 2)

;; Yaml
(require 'yaml-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
