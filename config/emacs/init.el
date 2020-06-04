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
;; base16 themes for emacs
(straight-use-package 'base16-theme)
(require 'base16-theme)
;; git manager
(straight-use-package 'magit)
;; undo-tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
;; dash
(straight-use-package 'dash)
;; monitor
(straight-use-package 'monitor)
;; goto-chg
(straight-use-package 'goto-chg)
;; vim-like bindings for emacs
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
;; vim motions without numbers
(straight-use-package 'evil-easymotion)
;; inline searching with the s key
(straight-use-package 'evil-snipe)
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
;; commenting with vim motions
(straight-use-package 'evil-commentary)
(evil-commentary-mode)
;; evil-integration for org-mode
(straight-use-package 'org-evil)
(require 'org-evil)
;; Fancy bullets in org-mode
(straight-use-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; mode-line customization with evil support
(straight-use-package 'telephone-line)
(require 'telephone-line)
;; smart managing of parentheses
(straight-use-package 'paredit)
;; make sexps easier to distinguish
(straight-use-package 'rainbow-delimiters)
;; epub reader
(straight-use-package 'nov)

;; Settings
;;=========

;; load .el files in /lisp
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'uniquify)
(ido-mode t)
(setq uniquify-buffer-name-style 'forward
      ido-enable-flex-matching t
      indent-tabs-mode nil
      mouse-yank-at-point t
      require-final-newline t
      inhibit-startup-screen t)

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
;; highlight current line
(global-hl-line-mode)
;; Turn off unbroken single line wrap indicators
(fringe-mode '(0 . 0))
;; Graphical mode tweaks
(when (display-graphic-p)
  ;; macOS-specific graphical settings
  (when (string-equal system-type 'darwin)
    (setq ns-use-srgb-colorspace nil))

  ;; enable base16-dracula theme for emacs
  (setq custom-safe-themes t)
  (load-theme 'base16-dracula t)

  ;; telephone-line settings
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left
        telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
        telephone-line-primary-right-separator 'telephone-line-tan-right
        telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  ;; Font settings
  (add-to-list 'default-frame-alist '(font . "Iosevka Medium-16")))

;; Load telephone-line
(telephone-line-mode 1)

;; Keep backups and autosaves in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; GPG encryption
(when (string-equal system-type 'darwin)
    (setq epa-pinentry-mode 'loopback))

;; eshell settings
;; eshell prompt
(defun my-system-name ()
  "Returns system-name without a '.*' suffix"
    (if (cl-search "." system-name)
	(car (split-string system-name "\\."))
      (system-name)))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (user-login-name) 'face '(:foreground "CadetBlue1"))
         (propertize "@" 'face '(:foreground "white"))
         (propertize (my-system-name) 'face '(:foreground "MediumPurple1"))
         " "
         (propertize (eshell/pwd) 'face '(:foreground "SpringGreen1"))
         " $ "))
      eshell-prompt-regexp "^.+?@.+? .+ \$ ")
;; Add to $PATH for eshell
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "~/.dotfiles/bin")

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

;; paredit settings
;; enable paredit in lisp modes
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; rainbow-delimiters settings
;; enable rainbow delimiters in lisp modes
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)

;; Python mode
;; Set default Python shell to python3
(setq python-shell-interpreter "python3")
;; C++ mode
;; Set indenting settings
(setq c-default-style "bsd" c-basic-offset 4)

;; Keybindings
;;============

;; Use command as meta if running on macOS
(when
    (and (string-equal system-type "darwin") (display-graphic-p)
         (setq mac-command-modifier 'meta
               mac-option-modifier nil)))

;; open init.el in a new window on the right
(global-set-key (kbd "C-c i") (lambda ()
                                  "Opens init.el in a new window on the right"
                                  (interactive)
                                  (split-window-right)
                                  (other-window 1)
                                  (find-file '"~/.emacs.d/init.el")))

;; avy bindings
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)

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
