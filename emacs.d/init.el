;; .emacs file
;; 3-3-2020

;; Straight.el package manager
;;============================

;; emacs package manager
(require 'package)

;; initialize straight.el
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

;; install packages
;; solarized theme for emacs
(straight-use-package
 '(el-patch :type git :host github :repo "bbatsov/solarized-emacs"))
;; git integration
(straight-use-package 'magit)
;; undo-tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
;; vim-like bindings for emacs
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
;; dash
(straight-use-package 'dash)
;; monitor
(straight-use-package 'monitor)
;; evil-integration for org-mode
(straight-use-package 'org-evil)
(require 'org-evil)

;; Settings
;;=========

;; enable better defaults
(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

;; turn on line numbers
(global-linum-mode 1)

;; highlight current lines
(global-hl-line-mode)

;; load solarized
(setq custom-safe-themes t)
(load-theme 'solarized-dark t)

;; Keybindings
;;============

;; Use command as meta and preserve default macOS alt-key behavior
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; open init.el in a new window on the right
(global-set-key (kbd "C-c i") (lambda ()
                                  "Opens init.el in a new window on the right"
                                  (interactive)
                                  (split-window-right)
                                  (other-window 1)
                                  (find-file `"~/.emacs.d/init.el")))

;; evil bindings
;; Type 'jk' to exit insert mode
(defun my-jk ()
  "When j is typed in evil insert mode, wait 0.5 seconds for a k before switching to normal mode"
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(evil-define-key 'insert 'global "j" 'my-jk)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Meslo LG M for Powerline")))))
