; .emacs file

;; emacs package manager
(require 'package)

; straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install packages
; Solarized theme for emacs
(straight-use-package
 '(el-patch :type git :host github :repo "bbatsov/solarized-emacs"))
; Git integration
(straight-use-package 'magit)

; Turn on line numbers
(global-linum-mode 1)

;; Load solarized
(setq custom-safe-themes t)
(load-theme 'solarized-dark t)

; Enable better defaults
(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Meslo LG M for Powerline")))))
