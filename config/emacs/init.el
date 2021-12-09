;;; init.el --- -*- lexical-binding: t; outline-minor-mode: t; -*-

;; 5-3-2020

;;; Commentary:

;; A personal Emacs config.

;; The config is split into multiple sections.  Use outline-minor-mode bindings
;; (C-c @) to move between these headings.  C-u 6 C-c @ C-q will only display
;; major headings.

;;; Code:

;;;; Initialization

;; add my folder of lisp programs from dotfiles
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; load machine-specific settings and customizations
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

;; Straight.el package manager
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

;; Macro for organized package configuration
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

;;;; Editor --- Related to editing and inserting text

;;;;; Editor settings

;; set macOS modifier keys
(when (eq system-type 'darwin)
  ;; command is switched with control
  (setq mac-right-option-modifier 'meta
        mac-right-command-modifier 'control
        mac-command-modifier 'control
        mac-control-modifier 'super))

;; don't fully display long lines
(setq-default truncate-lines t)

;; acceptable line length
(setq-default fill-column 80)

;; never use tabs
(setq-default indent-tabs-mode nil)

;; scroll a long line individually like nano, instead of vertically shifting the whole document
(setq auto-hscroll-mode 'current-line)

;; holding shift during navigation doesn't expand selection
(setq shift-select-mode nil)

;; scroll horizontally
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; highlight and auto-create matching parentheses
(show-paren-mode 1)
(electric-pair-mode)

;; display line numbers in programming buffers only
(defun t/line-number ()
  "Turn on line numbers except in minibuffer."
  (unless (minibufferp)
    (display-line-numbers-mode)))

(dolist (hook '(conf-mode-hook
                prog-mode-hook))
  (add-hook hook #'t/line-number))

;; highlight indentation
(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :bind ("C-c t i" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))


;;;;; Text manipulation

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

;; useful augments of default binds
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "C-w") #'t/kill-word-backward-or-region) ; kill word backward
;; built-in, unused commands
(global-set-key (kbd "C-z") #'zap-up-to-char)
(global-set-key (kbd "C-x C-b") #'ibuffer) ; list buffers
(global-set-key (kbd "C-c h v") #'set-variable) ; set variable
;; Toggles
(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c t m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t v") #'visual-line-mode)
(global-set-key (kbd "C-c t w") #'window-toggle-side-windows)
;; Applications
(global-set-key (kbd "C-c o c") #'calendar)

;;;;; Text navigation and search

;; isearch alternative across frame
(use-package avy
  :straight t
  :bind
  (("M-j" . #'avy-goto-char-timer)
   :map isearch-mode-map
   ("M-j" . #'avy-isearch)))

;;;; Buffers --- Display, navigation, and management of buffers and windows

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

;; Reenable commands
(put 'narrow-to-region 'disabled nil)

;; keep buffers updated when underlying file changes
(global-auto-revert-mode)

;; shortcut access to C-x binds
;; Super bindings are intended for quick, often-used global commands.
(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)
(global-set-key (kbd "s-4") ctl-x-4-map)
(global-set-key (kbd "s-5") ctl-x-5-map)
(global-set-key (kbd "s-0") #'delete-window)
(global-set-key (kbd "s-g") #'keyboard-quit)
(global-set-key (kbd "s-n") #'next-buffer)
(global-set-key (kbd "s-p") #'previous-buffer)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-u") #'revert-buffer)

;; ensure buffer titles are unique
(use-package uniquify
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-strip-common-suffix t
        uniquify-after-kill-buffer-p t))

;; ~M-o~ to change window
(use-package ace-window
  :straight t
  :config
  (setq aw-scope 'frame)
  :bind
  ("M-o" . #'ace-window))

;;;; Files

;; ~Neuter~ emacs's littering #habit#
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      custom-file (expand-file-name "custom.el" no-littering-etc-directory))

;; don't permanently delete files
(setq delete-by-moving-to-trash t)
(use-package osx-trash
  :if (eq 'system-type 'darwin)
  :straight t
  :defer 2
  :config
  (osx-trash-setup))

;; recently opened files
(use-package recentf
  :defer 1
  :config
  (recentf-mode))

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

;;;;; Version control

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

;; highlight changes in fringe
(use-package diff-hl
  :straight t
  :defer t
  :config
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;;;; Interface -- Emacs appearance and direct interaction

;; font settings
(add-to-list 'default-frame-alist '(font . "Menlo-12"))
(set-face-attribute 'default nil :family "Menlo" :height 120 :weight 'normal)
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 120)

;; minimalist emacs layout
(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; Remove prompts that explicitly require "yes" or "no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; scratch message on startup
(setq initial-scratch-message
      ";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with \\[find-file] and enter text
in its buffer.

")

;; none of us are immune to vanity
(use-package all-the-icons
  :straight t
  :defer t)

;;;;; Titlebar

;; frame title (buffer name, edit status, GNU Emacs version)
(setq-default frame-title-format '("%b %* GNU Emacs " :eval emacs-version))

;; macOS titlebar support
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :straight t
  :hook (emacs-startup . ns-auto-titlebar-mode))

;;;;; Theme

;; theme toggling with C-c h t
(defvar t/theme-list (list)
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
  (when (null t/theme-list)
    (error "Variable \"t/theme-list\" must be a list of themes"))
  (let ((list t/theme-list)
        (chosen-theme))
    ;; iterate through list until loaded theme is found or theme is empty
    (while (and (not (string= (car list)
                              t/theme--loaded))
                list)
      (setq list (cdr list)))
    ;; load the next theme, or use the first theme if no remaining elements in list
    (if (null (car (cdr list)))
        (setq chosen-theme (car t/theme-list))
      (setq chosen-theme (car (cdr list))))
    (t/load-theme chosen-theme)
    (setq t/theme--loaded chosen-theme))
  (message "Loaded theme %s" t/theme--loaded))

(global-set-key (kbd "C-c h t") #'t/cycle-themes)

;; theme of choice
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
        modus-themes-syntax '(yellow-comments alt-syntax))
  :config
  (dolist (theme '(modus-operandi modus-vivendi))
    (add-to-list 't/theme-list theme)))

;;;;; Modeline

;; spacemacs modeline
(use-package spaceline
  :straight t
  :init
  (setq ns-use-srgb-colorspace nil)
  :hook
  (emacs-startup . (lambda ()
                     (require 'spaceline-config)
                     (setq powerline-height 16
                           powerline-default-separator 'wave)
                     (spaceline-spacemacs-theme)
                     (spaceline-toggle-minor-modes-off))))

;;;;; Completion

;; built in completion
(use-package icomplete
  :init
  (defvar t/icomplete-page-scroll-margin 1
    "Number of entries that will stay on screen from last page during scroll.")
  (defun t/icomplete-next-page (arg)
    "Cycle completions forward by ARG pages."
    (interactive "p")
    (dotimes (n (- (* (or icomplete-vertical-prospects-height
                          icomplete-prospects-height)
                      arg)
                   t/icomplete-page-scroll-margin))
      (icomplete-forward-completions)))
  (defun t/icomplete-prev-page (arg)
    "Cycle completions backward by ARG pages."
    (interactive "p")
    (dotimes (n (- (* (or icomplete-vertical-prospects-height
                          icomplete-prospects-height)
                      arg)
                   t/icomplete-page-scroll-margin))
      (icomplete-backward-completions)))
  (icomplete-mode)
  :config
  (setq icomplete-scroll t
        icomplete-show-matches-on-no-input t
        icomplete-tidy-shadowed-file-names t
        icomplete-compute-delay 0)
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . t/icomplete-next-page)
              ("M-v" . t/icomplete-prev-page)
              ("RET" . icomplete-fido-ret) ; Return key executes candidate at point
              ("C-." . nil)))

;; vertical completion candidates like ivy/helm
(use-package icomplete-vertical
  :straight t
  :after icomplete
  :config
  (setq icomplete-vertical-prospects-height 15)
  (icomplete-vertical-mode))

;; sane fuzzy matching
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults '((file (styles partial-completion))))
  (savehist-mode))

;; candidate annotations
(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

;; completing extensions of regular commands
(use-package consult
  :straight t
  :bind
  (   ;; C-x bindings (ctl-x-map)
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

;; contextual actions on the thing at point
(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; dropdown completion (explicitly call with M-TAB)
(use-package corfu
  :straight t
  :hook
  ((prog-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :config
  ;; Corfu child frames don't display in terminals, so fall back to consult completion
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (display-graphic-p)
                     #'corfu--completion-in-region
                   #'consult-completion-in-region)
                 args))
        corfu-scroll-margin 0))

;;;;; Documentation

;; enhanced C-h help system
(use-package helpful
  :straight t
  :bind
  ("C-h k" . #'helpful-key)
  ("C-h v" . #'helpful-variable)
  ("C-h f" . #'helpful-callable))

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

;;;; Modes --- Filetype-specific settings

;; HTML browser
(use-package eww
  :defer t
  :bind ("C-c o w" . #'eww)
  :commands (eww))

;;;;; Prose Modes

(use-package text-mode
  :hook (org-mode . auto-fill-mode))

;; enhanced pdf viewing
(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;;;;;; Org-mode

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
      (call-interactively #'find-file)))
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
        ("C-s-e" . #'t/org-emphasize)
        ;; Search headings
        ("M-g o" . consult-org-heading)))

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
  :disabled
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
  :init
  (setq org-journal-file-format "%Y%m%d.org")
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

;; search plaintext files
(use-package deft
  :straight t
  :init
  (defun t/deft-kill-existing-buffer ()
    "Kill any existing deft buffer.
Return nil if no deft buffer exists."
    (let ((buf (get-buffer deft-buffer)))
      (if buf
          (kill-buffer buf)
        nil)))

  (defun t/deft-in-dir (dir)
    "Call a *deft* buffer limited to files in DIR.
Any existing deft buffer will be killed first.  If DIR is not a
valid directory, raise an error."
    (interactive "DSelect deft directory: ")
    (unless (f-dir-p dir)
      (error "%s is not a valid directory path" dir))
    (let ((deft-directory dir))
      (t/deft-kill-existing-buffer)
      (deft)))

  (defun t/deft-reset ()
    "Open deft, killing any existing deft buffer first."
    (interactive)
    (t/deft-in-dir deft-directory))

  (defun t/deft-in-roam-dir ()
    "Open a deft buffer limited to notes in \"org-roam-directory\"."
    (interactive)
    (t/deft-in-dir org-roam-directory))

  :bind
  ("<f8>" . deft)                       ; Display existing or new deft buffer
  ("C-c n s" . deft)
  ("C-c n S" . t/deft-in-dir)           ; Prompt for directory to open deft in
  ("C-<f8>" . t/deft-reset)             ; Open new deft buffer
  ("M-<f8>" . t/deft-in-roam-dir)       ; Open deft buffer limited to org-roam notes

  :config
  ;; following parser replacement from https://githubmemory.com/repo/jrblevin/deft/issues/75
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
	  (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	(deft-base-filename file))))
  
  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  (setq deft-directory org-directory
        deft-default-extension "org"
        deft-recursive t)
  ;; ensure :PROPERTIES: and lowercase #+props are not shown in note summaries
  (setq deft-strip-summary-regexp "\\(^:.*:.*\\|[\n	]\\|^#\\+[[:alpha:]_]+:.*$\\)"))

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

;;;;; Programming Modes

;; settings for editing c
(use-package cc-mode
  :defer t
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))
  (setq-default c-basic-offset 4)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; edit .yml files
(use-package yaml-mode
  :straight t
  :defer t)

;;;; External Processes

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; help emacs find executables
(dolist (path '("/Library/TeX/texbin/" "/usr/local/bin"))
  (add-to-list 'exec-path path))

(setenv "PKG_CONFIG_PATH"
        "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
(setenv "PATH"
        (s-join ":" exec-path))

;;;; Code

;; error checking
(use-package flycheck
  :straight t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; xref-backend that supports many languages
(use-package dumb-jump
  :straight t
  :commands t/maybe-lsp)

;;;;; Format

;; keep things aligned
(use-package aggressive-indent-mode
  :straight t
  :hook
  (prog-mode . aggressive-indent-mode))

;; automatic formatting of program buffers on save
(use-package format-all
  :straight t
  :commands format-all-mode
  :defer t)

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

;;;;; LSP

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

;;;;; Tree-sitter

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

;;;;; Tools

;; terminal emulator
(use-package vterm
  :straight t
  :defer t
  :commands vterm
  :bind ("C-c o t" . vterm))

(use-package eshell
  :defer t
  :init
  (setq eshell-banner-message "")
  :bind ("C-c o e" . eshell))

;; package manager
;; doesn't populate completions like helm-pac-man
;; maybe look at helm's implementation and recreate it with completing-read?
(use-package system-packages
  :straight t
  :config
  (setq system-packages-package-manager 'brew
        system-packages-use-sudo nil))

;;;; Email

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

;;;; Reenable automatic mode handling according to file

;; reminds user to keep good posture
(use-package posture
  :config
  (global-set-key (kbd "C-c t p") #'toggle-posture-reminder))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist t/file-name-handler-alist)))

;;; init.el ends here
