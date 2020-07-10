;; init.el file
;; 5-3-2020

;; Straight.el package manager
;;============================

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
(straight-use-package 'use-package)

;; base16 themes for emacs
(use-package base16-theme
  :straight t
  :init
    (when (string-equal system-type 'darwin)
      (setq ns-use-srgb-colorspace nil))
  :config
  (load-theme 'base16-dracula t))

;; git manager
(use-package magit
  :straight t)

;; undo-tree
(use-package undo-tree
  :straight t)
(global-undo-tree-mode)

;; dash
(use-package dash
  :straight t)

;; monitor
(use-package monitor
  :straight t)

;; goto-chg
(use-package goto-chg
  :straight t)

;; vim-like bindings for emacs
(use-package evil
  :straight t
  :config
  (evil-mode 1))

;; vim motions without numbers
(use-package evil-easymotion
  :straight t)

;; inline searching with the s key
(use-package evil-snipe
  :straight t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

;; commenting with vim motions
(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode))

;; evil-integration for org-mode
(use-package org-evil
  :straight t)

;; Fancy bullets in org-mode
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

;; smart managing of parentheses
(use-package paredit
  :straight t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup emacs-lisp-mode ielm-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

;; make sexps easier to distinguish
(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup emacs-lisp-mode ielm-mode lisp-mode lisp-interaction-mode scheme-mode) . rainbow-delimiters-mode))

;; fancy icons
(use-package all-the-icons
  :straight t)

;; epub reader
(use-package nov
  :straight t)

;; Settings
;;=========

;; load .el files in /lisp
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'mode-line)

(require 'uniquify)
(ido-mode t)
(setq uniquify-buffer-name-style 'forward
      ido-enable-flex-matching t
      indent-tabs-mode nil
      mouse-yank-at-point t
      require-final-newline t
      inhibit-startup-screen t
      default-directory "~")
;;      setq find-file-visit-truename t)

;; emacs appearance settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(show-paren-mode 1)
;; save and load point in files
(save-place-mode 1)
;; line numbers
(global-display-line-numbers-mode 1)
;; Turn off unbroken single line wrap indicators
(fringe-mode '(0 . 0))
;; highlight current line
(global-hl-line-mode)

;; Font settings
(add-to-list 'default-frame-alist '(font . "Iosevka Medium-12"))

;; Keep backups and autosaves in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; GPG encryption
(when (string-equal system-type 'darwin)
    (setq epa-pinentry-mode 'loopback))

;; eshell settings
(defvar is-eterm-buffer nil
  "If set to t, the function 'my/eshell-exit' will exit the current frame. Otherwise, it will run 'eshell/exit' and kill the current eshell buffer.")

(defun my/eterm ()
  "Opens separate frame specifically for eshell"
  (interactive)
  (make-frame-command)
  (set-frame-size (selected-frame) 90 24)
  (eshell 'new)
  (make-local-variable 'is-eterm-buffer)
  (setq mode-line-format nil)
  (setq-local is-eterm-buffer t)
  (select-frame-set-input-focus (selected-frame)))

(defun my/eshell-exit ()
  "Wrapper for eshell/exit that closes the frame if opened by my/eterm"
  (if is-eterm-buffer
      (delete-frame)
    (eshell/exit)))

(defun my/system-name ()
  "Returns system-name without a '.*' suffix"
    (if (cl-search "." system-name)
	(car (split-string system-name "\\."))
      (system-name)))

(defun my/pwd ()
  "Returns the last two or less directories of the current working directory"
  (let ((epwd (split-string (eshell/pwd) "/")))
    (if (<= (length epwd) 2)
	(mapconcat 'identity epwd "/")
      (mapconcat 'identity (last epwd 2) "/"))))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (user-login-name) 'face '(:foreground "CadetBlue1"))
         (propertize "@" 'face '(:foreground "white"))
         (propertize (my/system-name) 'face '(:foreground "MediumPurple1"))
         " "
         (propertize (my/pwd) 'face '(:foreground "SpringGreen1"))
         " λ "))
      eshell-prompt-regexp "^.+?@.+? .+ \λ ")

;; Add to $PATH for eshell
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "~/.dotfiles/bin")
(add-hook 'eshell-mode-hook '(lambda ()
			       (eshell/addpath "/usr/local/bin"
					       "/usr/local/sbin"
					       "~/.dotfiles/bin")
			       (set-window-margins nil 1)
			       (display-line-numbers-mode 0)))

;; Org-mode settings
;; Indent headings in org files
(setq org-startup-indented t)
;; Set image width
(setq org-image-actual-width nil)
;; Set font in Org buffers
(add-hook 'org-mode-hook (lambda ()
                            (setq buffer-face-mode-face '(:family "Iosevka Aile Medium"))
                            (buffer-face-mode)))
;; Turn off line numbers
(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode 0)))
;; Set visual-line-mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; Python mode
;; Set default Python shell to python3
(setq python-shell-interpreter "python3")
;; C++ mode
;; Set indenting settings
(setq c-default-style "bsd" c-basic-offset 4)

;; Keybindings
;;============

;; set right command to control on macOS
(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control))

;; open init.el in a new window on the right
(global-set-key (kbd "C-c i") (lambda ()
                                  "Opens init.el in a new window on the right"
                                  (interactive)
                                  (split-window-right)
                                  (other-window 1)
                                  (find-file '"~/.config/emacs/init.el")))

;; open an eshell instance in a new frame, mimicking a standard terminal
(global-set-key (kbd "C-c t") 'my/eterm)
;; use ibuffer to list buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;; easymotion binding
(evilem-default-keybindings "'")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my/evil-normal ((t (:background "dodger blue" :foreground "white" :weight ultra-bold))) t))
