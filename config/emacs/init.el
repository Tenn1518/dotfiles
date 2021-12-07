;;; init.el --- -*- lexical-binding: t; outline-minor-mode: t; -*-
;; 5-3-2020

;;; Commentary:

;; A personal Emacs config.

;; The config is split into multiple sections.
;; Use outline-minor-mode bindings (C-c @) to move between these headings.

;;; Code:

;;;; Initialization

;; load per-machine settings
(load (expand-file-name "local.el" user-emacs-directory))

;; Startup time notification
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq no-littering-etc-directory
      (expand-file-name "etc" user-emacs-directory)
      no-littering-var-directory
      (expand-file-name ".var" user-emacs-directory)
      straight-base-dir no-littering-var-directory)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" no-littering-var-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)

;; Clean up emacs folder
(use-package no-littering
  :straight t)

;; Only garbage collect when idle
(use-package gcmh
  :straight t
  :demand t
  :config
  (gcmh-mode 1))

;;;; Appearance

;; font settings
(add-to-list 'default-frame-alist '(font . "Menlo-12"))
(set-face-attribute 'default nil :family "Menlo" :height 120 :weight 'normal)
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 120)

;; ensure titles are unique
(use-package uniquify
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward))

;; minimalist emacs layout
(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ~Neuter~ emacs's littering #habit#
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      custom-file (expand-file-name "custom.el" no-littering-etc-directory))

;; display line numbers in programming buffers only
(defun t/line-number ()
  "Turn on line numbers except in minibuffer."
  (unless (minibufferp)
    (display-line-numbers-mode)))
(add-hook 'conf-mode-hook 't/line-number)
(add-hook 'prog-mode-hook 't/line-number)

;; scroll a long line individually like nano, instead of vertically shifting the whole document
(setq auto-hscroll-mode 'current-line)

;; frame title (buffer name, edit status, GNU Emacs version)
(setq-default frame-title-format '("%b %* GNU Emacs " :eval emacs-version))

;; scratch message on startup
(setq initial-scratch-message
      ";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with \\[find-file] and enter text
in its buffer.

")

;; catch user's eye when moving across the screen
(use-package pulse
  :defer t
  :disabled
  :config
  (defun t/pulse-line (&rest args)
    (if (not (minibufferp))
        (pulse-momentary-highlight-one-line (point))))
  (global-set-key (kbd "<s-escape>") #'t/pulse-line)
  (set-face-attribute 'pulse-highlight-face nil :background "#f7d7fa")
  (set-face-attribute 'pulse-highlight-start-face nil :background "#f7d7fa")
  (advice-add 'select-window :after-while #'t/pulse-line))

;; macOS titlebar support
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :straight t
  :hook (emacs-startup . ns-auto-titlebar-mode))

;; theme toggling with C-c h t
(defvar t/theme-list '(modus-operandi
                       modus-vivendi)
  "List of themes t/cycle-themes cycles through.")

(defvar t/theme--loaded nil
  "Currently loaded theme.")

(defun t/load-theme (THEME)
  "Wrapper for \"load-theme\".  Variable \"t/theme--loaded\" is set to THEME upon use."
  (load-theme THEME t)
  (setq t/theme--loaded THEME)
  ;; recompile spaceline if present
  (when (require 'spaceline nil t)
    (spaceline-compile)))

(defun t/cycle-themes ()
  "Cycle themes according to the elements in t/theme-list."
  (interactive)
  (let ((list t/theme-list)
        (chosen-theme))
    ;; iterate through list until loaded theme is found or theme is empty
    (while (and (not (string= (car list)
                              t/theme--loaded))
                (not (equal list nil)))
      (setq list (cdr list)))
    ;; load the next theme, or use the first theme if no remaining elements in list
    (if (equal (car (cdr list)) nil)
        (setq chosen-theme (car t/theme-list))
      (setq chosen-theme (car (cdr list))))
    (t/load-theme chosen-theme)
    (setq t/theme--loaded chosen-theme))
  (message "Loaded theme %s" t/theme--loaded))
(global-set-key (kbd "C-c h t") #'t/cycle-themes)

;; theme
(use-package modus-themes
  :straight t
  :hook (emacs-startup . (lambda () (t/load-theme 'modus-operandi)))
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-syntax '(alt-syntax)
        modus-themes-scale-title 1.4
        modus-themes-scale-4 1.35
        modus-themes-scale-3 1.2
        modus-themes-scale-2 1.1
        modus-themes-scale-1 1.05
        modus-themes-syntax '(yellow-comments alt-syntax)))

;; none of us are immune to vanity
(use-package all-the-icons
  :straight t
  :defer t)

;; spacemacs modeline
(use-package spaceline
  :straight t
  :hook
  (emacs-startup . (lambda ()
                     (require 'spaceline-config)
                     (setq powerline-height 16
                           powerline-default-separator 'wave)
                     (spaceline-spacemacs-theme)
                     (spaceline-toggle-minor-modes-off))))

;;;; Emacs Lisp settings

;; add my folder of lisp programs from dotfiles
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Reenable commands
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; enhanced C-h help system
(use-package helpful
  :straight t
  :bind
  ("C-h k" . #'helpful-key)
  ("C-h v" . #'helpful-variable)
  ("C-h f" . #'helpful-callable))

;;;; Navigation/Editing

;; acceptable line length
(setq-default fill-column 80)

;; never use tabs
(setq-default indent-tabs-mode nil)

;; don't permanently delete files
(setq delete-by-moving-to-trash t)
(use-package osx-trash
  :if (eq 'system-type 'darwin)
  :straight t
  :defer 2
  :config
  (osx-trash-setup))

;; set macOS modifier keys
(when (eq system-type 'darwin)
  ;; command is switched with control
  (setq mac-right-option-modifier 'meta
        mac-right-command-modifier 'control
        mac-command-modifier 'control
        mac-control-modifier 'super))

;; shortcut access to C-x binds
;; Super bindings are intended for quick, often-used global commands.
;; Prioritize adding smart commands that do thinking for me.
(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)
(global-set-key (kbd "s-4") ctl-x-4-map)
(global-set-key (kbd "s-5") ctl-x-5-map)
(global-set-key (kbd "s-0") #'delete-window)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-u") #'revert-buffer)
(global-set-key (kbd "s-g") #'keyboard-quit)

;; useful augments of default binds
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)

;; replaces unnecessary suspend command
(global-set-key (kbd "C-z") #'zap-up-to-char)

;; ibuffer for listing buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Set Emacs variables interactively
(global-set-key (kbd "C-c h v") #'set-variable)

;; Edit emacs directory
(defun t/edit-emacs-dir ()
  "Edit file from Emacs configuration directory."
  (interactive)
  (find-file user-emacs-directory))
;; (global-set-key (kbd "C-c f p") #'t/edit-emacs-dir)

;; Open scratch buffer
(defun t/open-scratch ()
  "Find and open the scratch buffer."
  (interactive)
  (pop-to-buffer "*scratch*"))
(global-set-key (kbd "C-c X") #'t/open-scratch)

;; Kill word backward with C-w like vim/GNU Readline
(defun t/kill-word-backward-or-region (arg)
  "Kill the contents of the region, if active.  If not, kill ARG words backwards."
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") #'t/kill-word-backward-or-region)

;; Toggles
(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c t m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t v") #'visual-line-mode)

;; Applications
(global-set-key (kbd "C-c o c") #'calendar)

;; set rules for displaying buffers
(setq display-buffer-alist
      '(("^\\*eshell.*\\*$"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (slot . -1)
         (window-width . .3)
         (window-height . .3)
         (window-parameters . ((mode-line-format . none))))
        ("^\\*vterm.*\\*$"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (slot . -1)
         (window-width . .3)
         (window-height . .3)
         (window-parameters . ((mode-line-format . none))))
        ("(\\*Messages\\*|Output\\*$)"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (height . .3))
        ("\\(^\\*helpful\\|\\*Help\\*\\)"
         (display-buffer-reuse-mode-window
          display-buffer-pop-up-window
          display-buffer-in-side-window)
         (slot . 1)
         (mode . (helpful-mode help-mode))
         (window-height . .3))
        ("\\(^magit:\\|\\*info\\*\\|NEWS\\)"
         (display-buffer-reuse-window
          display-buffer-use-some-window
          display-buffer-in-direction)
         (direction . right)
         (split-width-threshold . .5)
         (window-width . .5)
         (inhibit-same-window . t))
        ("\\*org-roam\\*"
         (display-buffer-in-side-window)
         (side . right)
         (slot . 0)
         (window-width . .25)
         (preserve-size . (t nil)))))
;; toggle term and help windows
(global-set-key (kbd "C-c t w") #'window-toggle-side-windows)

;; expedites copying from org-mode to Word for essays
(defun t/make-region-pastable ()
  "Save content of the region to the kill ring.
The copy is altered to remove extraneous newlines and double
spaces."
  (interactive)
  (let* ((buf (current-buffer))
         (beg (region-beginning))
         (end (region-end)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf beg end)
      (goto-char (point-min))
      ;; Deletes newlines within a single paragraph
      (while (re-search-forward "\[a-zA-Z.\]\\(\n\\)\\w" nil t)
        (replace-match " " nil nil nil 1))
      ;; Single space over double space sentences
      (replace-string ".  " ". " nil (point-min) (point-max))
      ;; Delete empty line between paragraphs
      (replace-string "\n\n" "\n" nil (point-min) (point-max))
      (kill-new (buffer-string) t)))
  (setq deactivate-mark t)
  (message "Region copied to clipboard"))
;;(global-set-key (kbd "C-c p") #'t/make-region-pastable)

;; file manager
(use-package dired
  :config
  ;; point dired to correct ls binary on macOS
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))
  ;; hide dotfiles with C-x M-o
  (setq dired-omit-files "^\\..\\{2,\\}")  ; default: "\\`[.]?#\\|\\`[.][.]?\\'"
  :bind ("C-x C-j" . #'dired-jump))

;; ~M-o~ to change window
(use-package ace-window
  :straight t
  :config
  (setq aw-scope 'frame)
  :bind
  ("M-o" . #'ace-window))

;; isearch alternative across frame
(use-package avy
  :straight t
  :bind
  (("M-j" . #'avy-goto-char-timer)
   :map isearch-mode-map
   ("M-j" . #'avy-isearch)))

;; available keybinds popup with "C-h" during key chords
(use-package which-key
  :straight t
  :config
  (setq
   ;; Only show popup on "C-h"
   which-key-show-early-on-C-h t
   which-key-idle-delay 10000
   which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  ;; prefer right side, then bottom for popup
  (which-key-setup-side-window-right-bottom)
  :bind ("C-c h k" . #'which-key-show-top-level))

;; file explorer window
(use-package treemacs
  :straight t
  :config
  (setq treemacs-space-between-root-nodes nil
        treemacs-width 30)
  (dolist (face '(treemacs-file-face
                  treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-tags-face
                  treemacs-root-face
                  treemacs-git-unmodified-face
                  treemacs-git-modified-face
                  treemacs-git-renamed-face
                  treemacs-git-ignored-face
                  treemacs-git-untracked-face
                  treemacs-git-added-face
                  treemacs-git-conflict-face))
    (set-face-attribute face nil :inherit 'variable-pitch))
  :bind ("C-c t t" . #'treemacs))
(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs)
  :config
  (treemacs-load-theme 'all-the-icons))
(use-package treemacs-magit
  :straight t
  :after (treemacs))
(use-package treemacs-projectile
  :straight t
  :after (treemacs))

(use-package recentf
  :defer 1
  :config
  (recentf-mode))

;;;; Completion

(use-package vertico
  :straight t
  :config
  (setq completion-in-region-function   ; C-M-i to complete
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (setq vertico-scroll-margin 2
        vertico-count 20
        vertico-resize nil
        vertico-cycle nil)
  (vertico-mode))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless))
  (savehist-mode))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package consult
  :straight t
  :bind
  (;; C-c bindings (mode-specific-map)
   ;;("C-c h" . consult-history)
   ;;("C-c m" . consult-mode-command)
   ;; ("C-c b" . consult-bookmark)
   ;; ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g O" . consult-org-heading)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s f" . consult-find)
   ("M-s F" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("s-s" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)))

;; select from useful directories during minibuffer commands
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (setq consult-dir-project-list-function 'nil))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Programming

;; allow subprocesses more data at once (lsp)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; help emacs find executables
(add-to-list 'exec-path "/Library/TeX/texbin/")
(add-to-list 'exec-path "/usr/local/bin")

;; highlight and auto-create matching parentheses
(show-paren-mode 1)
(electric-pair-mode)

;; keep buffers updated with file changes
(global-auto-revert-mode)

;; don't fully display long lines
(setq-default truncate-lines t)

;; display region when active
(transient-mark-mode)

;; settings for editing c
(use-package cc-mode
  :defer t
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))
  (setq-default c-basic-offset 4))

;; view imenu in a dedicated popup buffer
(use-package imenu-list
  :straight t
  :defer t
  :bind ("C-c t I" . #'imenu-list-smart-toggle))

;; git frontend
(use-package magit
  :straight t
  :defer t
  :bind (("C-c o g" . #'magit-status)
         ("<f5>" . #'magit-status)))

;; built in version control
(use-package diff-mode
  :config
  (define-key diff-mode-map (kbd "M-o") nil))

;; automatic formatting of program buffers on save
(use-package format-all
  :straight t
  :commands format-all-mode
  :defer t)

;; xref-backend that supports many languages
(use-package dumb-jump
  :straight t
  :commands t/maybe-lsp)

;; IDE-like features through language servers
(use-package lsp-mode
  :straight t
  :defer t
  :commands lsp lsp-deferred
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c c")
  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  (defvar t/lsp-enabled-modes
    '(python-mode c++-mode js-mode)
    "List of major modes which LSP should activate on start.")
  (defun t/maybe-lsp ()
    "Enable lsp-mode if the current major mode is included in
t/lsp-enabled-modes. Otherwise, enable format-all-mode and use
dumb-jump as the xref backend."
    (cond ((member major-mode t/lsp-enabled-modes)
           (lsp))
          ((derived-mode-p 'prog-mode)
           (progn (when (require 'format-all nil t)
                    (format-all-mode t))
                  (when (require 'dumb-jump nil t)
                    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t))))))
  (defun t/lsp-onsave ()
    "Configure LSP to automatically format all code on save in
lsp-enabled buffers."
    (lsp-enable-which-key-integration)
    (cond ((derived-mode-p 'c++-mode)
           (add-hook 'before-save-hook #'clang-format-buffer nil 'local))
          (t (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))
    (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))
  :hook
  ((prog-mode . t/maybe-lsp)
   (lsp-mode . t/lsp-onsave)))

;; UI frontend for language server alerts and info
(use-package lsp-ui
  :straight t
  :defer t
  :after lsp
  :config
  (setq lsp-ui-doc-position 'bottom))

;; python language server
(use-package lsp-python-ms
  :straight t
  :defer t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))
;; format python buffers with black
(use-package blacken
  :straight t
  :defer t
  :hook (python-mode . blacken-mode))

;; clang-format
(use-package clang-format
  :straight t
  :defer t
  :commands (clang-format-buffer))

;; error checking
(use-package flycheck
  :straight t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; highlight changes in fringe
(use-package diff-hl
  :straight t
  :defer t
  :config
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; highlight indentation
(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :bind ("C-c t i" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; tree sitter
(use-package tree-sitter
  :straight t
  :hook
  ((prog-mode . turn-on-tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode)))
(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)
(use-package tree-sitter-indent
  :straight t
  :after tree-sitter)

;; edit .yml files
(use-package yaml-mode
  :straight t
  :defer t)

;; terminal emulator
(use-package vterm
  :straight t
  :defer t
  :commands vterm
  :bind ("C-c o t" . vterm))
(use-package eshell
  :defer t
  :bind ("C-c o e" . eshell))

;; package manager
;; doesn't populate completions like helm-pac-man
;; maybe look at helm's implementation and recreate it with completing-read?
(use-package system-packages
  :straight t
  :config
  (setq system-packages-package-manager 'brew
        system-packages-use-sudo nil))

;;;; Org-mode

(use-package text-mode
  :hook (org-mode . auto-fill-mode))

(use-package org
  :straight t
  :defer t
  :init
  (setq
   ;; relevant paths
   org-directory "~/Documents/Notes/"
   t/daily-file (expand-file-name "daily.org" org-directory)
   ;; settings
   org-startup-with-inline-images t
   org-startup-with-latex-preview t
   org-archive-default-command #'org-archive-to-archive-sibling
   org-id-track-globally t
   org-imenu-depth 7
   org-use-speed-commands t)
  ;; open file from org-directory
  (defun t/edit-org-dir ()
    (interactive)
    (let ((default-directory org-directory))
      (call-interactively #'helm-find-files)))
  ;; org-emphasis modification
  ;; default to word if no region
  (defun t/org-emphasize ()
    "Insert or change an emphasis, defaulting to the word at
point if no selection."
    (interactive)
    (unless (region-active-p)
      (mark-word))
    (call-interactively #'org-emphasize))
  ;; Passed as an argument to org-capture-templates's file+function to replace
  ;; the standard datetree.  It does not create a tree of year and month, only a
  ;; heading for day.
  (defun t/org-date-find ()
    "Find or create a level 1 heading titled with current date.
This function is intended to be passed as an argument to
file+function in org-capture-templates."
    (let ((date (format-time-string "%Y-%m-%d %A")))
      (condition-case nil
          (goto-char (org-find-olp `(,date) t))
        (t (progn
             (goto-char (point-min))
             (org-next-visible-heading 1)
             (move-beginning-of-line nil)
             (org-insert-heading nil t)
             (insert date))))))
  ;; tags and captures
  (setq org-tag-persistent-alist
        '(("note")
          ("event")
          ("task")
          ("school"))
        org-capture-templates
        `(("t" "Quick todo" entry
           (file+function t/daily-file t/org-date-find)
           "* TODO %?\n%U"
           :empty-lines 1 :prepend t)
          ("n" "Quick note" entry
           (file+function t/daily-file t/org-date-find)
           "* UNREAD %? :note:\n%U"
           :empty-lines 1 :prepend t)
          ("e" "Upcoming event" entry
           (file+function t/daily-file t/org-date-find)
           "* EVENT %? :event:\nSCHEDULED: %^{Event Date}t"
           :empty-lines 1 :prepend t)
          ;; entries for school file
          ("s" "Templates for school")
          ("st" "Class-related todo" entry
           (file+function t/daily-file t/org-date-find)
           ,(concat "* TODO %^{Title} :hw:\n"
                    "DEADLINE:%^{Deadline}t\n\n"
                    "+ [[%?][Turn-In]]")
           :empty-lines 1)
          ("sn" "Class-related notes" entry
           (file+function t/daily-file t/org-date-find)
           "* %^{Title} :%^{Type|notes|class-info}:\n%U\n\n+ %?"
           :empty-lines 1)
          ("sp" "Class-related notes with associated document" entry
           (file+function t/daily-file t/org-date-find)
           "* %^{Title} :notes:\n:PROPERTIES:\n:NOTER_DOCUMENT: %F\n:END:\n%U\n\n%?")))
  :config
  (setq ;; appearance settings
   org-hide-leading-stars nil
   org-hide-emphasis-markers t
   org-startup-indented nil
   ;; latex settings
   org-preview-latex-image-directory (concat "~/.cache/emacs/" "ltximg")
   org-format-latex-options '( :foreground "Black"
                                           :background "White"
                                           :scale 1.5
                                           :html-foreground "Black"
                                           :html-background "Transparent"
                                           :html-scale 1.0
                                           :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
   ;; available todo states
   org-todo-keywords '((sequence "TODO(t)"
                                 "NEXT(n)"
                                 "IN-PROGRESS(i)"
                                 "WAITING(w)"
                                 "UPCOMING"
                                 "|"
                                 "DONE(d)"
                                 "CANCELLED(c!)")
                       (sequence "UNREAD(u)"
                                 "READ(r)")
                       (sequence "EVENT(e)"
                                 "OVER(o)")))
  :bind
  ;; C-c global bindings
  ("C-c n f" . #'t/edit-org-dir)
  ("C-c x" . #'org-capture)
  ("C-c l" . #'org-store-link)
  ;; edit/nav bindings in org-mode
  (:map org-mode-map
        ;; Move headings
        ("C-s-f" . #'org-metaright)
        ("C-s-b" . #'org-metaleft)
        ("C-s-p" . #'org-metaup)
        ("C-s-n" . #'org-metadown)
        ("C-S-s-f" . #'org-shiftmetaright)
        ("C-S-s-b" . #'org-shiftmetaleft)
        ("C-S-s-p" . #'org-shiftmetaup)
        ("C-S-s-n" . #'org-shiftmetadown)
        ;; Make item at point heading
        ("C-s-h" . #'org-toggle-heading)
        ;; Cycle bullet of current list
        ("C-s-<tab>" . #'org-cycle-list-bullet)
        ;; surround region or word at point in emphasis markers
        ("C-s-e" . #'t/org-emphasize)))

(use-package org-agenda
  :defer t
  :after org
  :init
  (defun t/org-agenda-view ()
    "Directly open org-agenda to the default agenda view."
    (interactive)
    (org-agenda nil "a"))
  :config
  (setq org-agenda-files (list org-directory)
        org-deadline-warning-days 3
        org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((tags-todo "task")
            (agenda ""))
           ((org-agenda-span 7)))
          ("w" "School" tags-todo "@school"
           ((org-agenda-span 5)
            (org-agenda-start-on-weekday 1)
            (org-agenda-time-grid nil)))))
  :bind (("C-c n a" . #'org-agenda)
         ("<f6>" . #'t/org-agenda-view)))

;; add latex class for exports
(use-package ox-latex
  :defer t
  :after org
  :config
  (add-to-list 'org-latex-classes
               '("apa6"
                 "\\documentclass{apa6}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; syntax highlighting in exported html
(use-package htmlize
  :straight t
  :after (org)
  :defer t)

;; smarter variable pitch
(use-package org-variable-pitch
  :straight t
  :after org
  :defer t
  :config
  (org-variable-pitch-setup)
  :bind
  (:map org-mode-map
        ("C-s-v" . org-variable-pitch-minor-mode)))

;; journal
(use-package org-journal
  :straight t
  :bind ("C-c n j" . org-journal-new-entry)
  :after org
  :defer t
  :config
  (setq org-journal-dir (expand-file-name (concat org-directory "journal/"))
        ;; don't open new window
        org-journal-find-file #'find-file)
  (push org-journal-dir org-agenda-files))

;; atomic note taker
(use-package org-roam
  :straight t
  :defer t
  :init
  (setq org-roam-directory (concat org-directory "roam")
        org-roam-v2-ack t ;; silence upgrade warning
        ;; org-roam-capture-templates
        ;; '(( "d"
        ;;     "default"
        ;;     plain #'org-roam-capture--get-point
        ;;     "%?"
        ;;     :file-name "%<%Y%m%d%H%M%S>-${slug}"
        ;;     :head "#+title: ${title}\n#+STARTUP: showeverything"
        ;;     :unnarrowed t)
        ;;   ( "m"
        ;;     "math note"
        ;;     plain #'org-roam-capture--get-point
        ;;     "%?"
        ;;     :file-name "%<%Y%m%d%H%M%S>-${slug}"
        ;;     :head "#+title: ${title}\n#+roam_tags: math\n#+STARTUP: latexpreview showeverything"
        ;;     :unnarrowed t)
        ;;   ( "s"
        ;;     "school note"
        ;;     plain #'org-roam-capture--get-point
        ;;     "%?"
        ;;     :file-name "%<%Y%m%d%H%M%S>-${slug}"
        ;;     :head "#+title: ${title}\n#+roam_tags: %^{engl|fsci|pltw|art|gym|phys|psych}\n#+STARTUP: showeverything"
        ;;     :unnarrowed t))
        )
  :bind
  ("C-c n r f" . #'org-roam-node-find)
  ("C-c n r i" . #'org-roam-node-insert)
  ("C-c n r I" . #'org-id-get-create)
  ("C-c n r n" . #'org-roam-capture)
  ("C-c n r r" . #'org-roam-buffer-toggle)
  :config
  (org-roam-db-autosync-mode))

;; browser page for viewing/navigating org-roam notes
(use-package org-roam-ui
  :disabled
  :straight t)
(use-package websocket
  :disabled
  :after org-roam)

;; cite sources from .bib files
(use-package org-ref
  :straight t
  :defer t
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-export-latex-format-toc-function 'org-export-latex-no-toc
        org-ref-get-pdf-filename-function
        (lambda (key) (car (bibtex-completion-find-pdf key)))
        ;; For pdf export engines
        org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
        ;; TODO: Consider orb to link org-ref, helm-bibtex and org-noter together
        ;;org-ref-notes-function 'orb-edit-notes)
        org-ref-default-bibliography "~/Zotero/zotero/zotero.bib"
        org-ref-default-citation-link "citep"))

;; taking notes from a document
(use-package org-noter
  :straight t
  :defer t
  :bind
  ("C-c n n" . #'org-noter))

;;;; Media viewing

;; enhanced pdf viewing
(use-package pdf-tools
  :straight t
  :config
  (setenv "PKG_CONFIG_PATH"
          "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  (setenv "PATH"
          (s-join ":" exec-path))
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; HTML browser
(use-package eww
  :defer t
  :bind ("C-c o w" . #'eww)
  :commands (eww))

;; view email; requires installing mu (maildir-utils on some distros) first
(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e" ; macOS-specific?
  :defer t
  :bind
  ("<f7>" . mu4e)
  ("C-c o m" . mu4e)
  :config
  (setq
   mail-user-agent 'mu4e-user-agent
   mu4e-get-mail-command "mbsync gmail"
   ;; Gmail does this itself
   mu4e-sent-messages-behavior 'delete))

;; send email
;; Settings necessary for sending mail are located in local.el
;; Copy TEMPLATE-local.el to local.el and fill in information
(use-package smtpmail
  :bind ("C-x m" . compose-mail))

;;;; Miscellaneous

;; reminds user to keep good posture
(use-package posture
  :config
  (global-set-key (kbd "C-c t p") #'toggle-posture-reminder))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist t/file-name-handler-alist)))

;;; init.el ends here
