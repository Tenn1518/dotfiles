;; init.el file
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
;; base16 theme for emacs
(straight-use-package 'base16-theme)
(require 'base16-theme)
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
;; Fancy bullets in org-mode
(straight-use-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; telephone-line
(straight-use-package 'telephone-line)
(require 'telephone-line)

;; Settings
;;=========

;; Stop the default startup screen
(setq inhibit-startup-screen t)

;; enable better defaults
(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

;; emacs appearance settings
;; turn on line numbers
(global-linum-mode 1)
;; highlight current lines
(global-hl-line-mode)
;; fix emacs.app colors
(setq ns-use-srgb-colorspace nil)
;; enable base16-dracula theme for emacs
(setq custom-safe-themes t)
(load-theme 'base16-dracula t)
;; telephone-line settings
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode 1)

;; Org-mode settings
;; Indent headings in org files
(setq org-startup-indented t)
;; Set to the location of your Org files on your local system
(setq org-directory '"~/Documents/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Documents/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

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

(evil-define-key '(insert) 'global "j" 'my-jk)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Meslo LG M for Powerline")))))
