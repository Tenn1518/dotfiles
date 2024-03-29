;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

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

;; List of modules to be loaded on startup
(defvar t/modules (list)
  "List of modules to be loaded on startup.
Modules are found in the base of the Emacs configuration
directory.  They are named init-MODULE.el.")

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
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         no-littering-var-directory))
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
;; TODO: crashes when gc-ing on Emacs 28
(use-package gcmh
  :disabled
  :straight t
  :demand t
  :config
  (gcmh-mode 1))

;; `M-x esup' to profile startup time
(use-package esup
  :straight t
  :defer t)

;;;; Editor --- Related to editing and inserting text

;;;;; Editor settings

;; set macOS modifier keys
(when (eq system-type 'darwin)
  ;; command is switched with control
  (setq mac-right-option-modifier 'meta
        mac-right-command-modifier 'super
        mac-command-modifier 'super
        mac-control-modifier 'control))

;; don't fully display long lines
(setq-default truncate-lines t)

;; acceptable line length
(setq-default fill-column 80)

;; never use tabs
(setq-default indent-tabs-mode nil)

;; necessary for sentence navigation commands to differentiate between
;; abbreviations (i.e. U.S.) and the end of a sentence
(setq sentence-end-double-space t)

;; scroll a long line individually like nano, instead of vertically shifting the
;; whole document
(setq auto-hscroll-mode 'current-line)

;; point won't move on scroll
(setq scroll-preserve-screen-position t)

;; holding shift during navigation doesn't expand selection
(setq shift-select-mode nil)

;; scroll horizontally
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; scrolling lines is animated to look smoother
;; (pixel-scroll-mode -1)

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

;; One stop region command
(use-package expand-region
  :straight t
  :config
  (global-set-key (kbd "C-M-SPC") #'er/expand-region))

;; Edit many at once
(use-package multiple-cursors
  :straight t
  :config
  (global-set-key (kbd "C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; highlight indentation
(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (general-def t/leader-toggle-map "i" #'highlight-indent-guides-mode))

;; shorten repeatable commands
(repeat-mode 1)
(setq repeat-keep-prefix t)

;; evil-friendly keybind macros
(use-package general
  :straight t
  :demand t

  :init
  ;; leader key (SPC in Evil mode, C-c in Emacs
  (general-create-definer t/leader-def
    :states '(normal insert)
    :keymaps '(global override)
    :prefix "SPC"
    :prefix-command 't/leader-map)
  (t/leader-def
    "t" '(:prefix-command t/leader-toggle-map :which-key "toggle")
    "h" '(:prefix-command t/leader-emacs-map :which-key "emacs")
    "n" '(:prefix-command t/leader-notes-map :which-key "notes")
    "o" '(:prefix-command t/leader-open-map :wk "open"))
  (let ((m t/leader-map))
    (define-key m "t" t/leader-toggle-map)
    (define-key m "h" t/leader-emacs-map)
    (define-key m "n" t/leader-notes-map)
    (define-key m "o" t/leader-open-map))
  ;; ensure compat if evil-mode isn't loaded
  ;; TODO: redundant `C-c s'/`M-s s' and `C-c p'/`C-x p' type binds?
  ;; possible solution: only bind Leader+s when evil loads
  (general-def "C-c" t/leader-map)
  ;; Replace `C-u' clobbered by Evil
  (t/leader-def "u" #'universal-argument))

;; Load evil-mode settings
(if (member 'evil t/modules)
    (load (expand-file-name "init-evil.el" user-emacs-directory)))

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
(t/leader-def
  "X" #'t/open-scratch)

(defun t/kill-word-backward-or-region (arg)
  "Kill the contents of the region, if active.
If not, kill ARG words backwards."
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

;; useful augments of default binds
(general-def
  "M-SPC" #'cycle-spacing
  "M-c" #'capitalize-dwim
  "M-u" #'upcase-dwim
  "M-l" #'downcase-dwim
  "C-w" #'t/kill-word-backward-or-region ; vim-like C-w except if region active
  "C-M-<backspace>" #'backward-kill-sexp
  ;; built-in, unused commands
  "C-z" #'zap-up-to-char
  "C-x C-b" #'ibuffer)
(general-def t/leader-toggle-map
  ;; Toggles
  "l" #'display-line-numbers-mode
  "v" #'visual-line-mode)
(general-def t/leader-open-map
  ;; Applications
  "c" #'calendar)
(general-def t/leader-emacs-map
  "v" #'set-variable)

;;;;; Text navigation and search

;; isearch alternative across frame
(use-package avy
  :disabled
  :straight t
  :defer t

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

;; update buffers with changes to the underlying file
(global-auto-revert-mode)

;; shortcut access to C-x binds
;; Super bindings are intended for quick, often-used global commands.
(general-def
  "s-1" #'delete-other-windows
  "s-2" #'split-window-below
  "s-3" #'split-window-right
  "s-4" ctl-x-4-map
  "s-5" ctl-x-5-map
  "s-0" #'delete-window
  "s-g" #'keyboard-quit
  "s-n" #'next-buffer
  "s-p" #'previous-buffer
  "s-o" #'other-window
  "s-u" #'revert-buffer)

;; leader keybinds for buffers
(t/leader-def "b" #'consult-buffer)
;; scroll buffer in two separate windows
(define-key t/leader-toggle-map (kbd "F") #'follow-mode)

;; project commands under <leader>p
(t/leader-def "p" project-prefix-map)

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
  :defer t

  :config
  ;; point dired to correct ls binary on macOS
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-aBhl --group-directories-first")
  ;; hide dotfiles with C-x M-o
  (setq dired-omit-files "^\\..\\{2,\\}"))  ; default: "\\`[.]?#\\|\\`[.][.]?\\'"

;; file explorer window
(use-package treemacs
  :straight t
  :defer t

  :init
  (define-key t/leader-toggle-map "t" #'treemacs)

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
    (set-face-attribute face nil :inherit 'variable-pitch)))
(use-package treemacs-all-the-icons
  :straight t
  :defer t
  :after treemacs
  :config
  (treemacs-load-theme 'all-the-icons))
(use-package treemacs-magit
  :straight t
  :defer t
  :after (treemacs))

;;;;; Version control

;; git frontend
(use-package magit
  :straight t
  :defer t

  :config
  (t/leader-def "gg" #'magit-status))

;; built in version control
(use-package diff-mode
  :defer t
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
;; (add-to-list 'default-frame-alist '(font . "Meslo LG S-10"))
(set-face-attribute 'default nil
                    :family "Noto Sans Mono"
                    ;; :height 100
                    :weight 'normal)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :height 140)
(set-face-attribute 'fixed-pitch nil :family "Meslo LG M")
(setq-default line-spacing 1)

;; Remove prompts that explicitly require "yes" or "no"
;;(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

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

;;;;; Frame

;; not a terminal
(setq frame-resize-pixelwise t)

;; frame title (buffer name, edit status, GNU Emacs version)
(setq-default frame-title-format '("%b %* GNU Emacs " :eval emacs-version))

;; macOS titlebar support
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :straight t
  :hook (emacs-startup . ns-auto-titlebar-mode))

;; workspaces and a global mode line
(use-package tab-bar
  :init
  (setq tab-bar-show 1)                 ; only show tabs if >1 tabs open

  :config
  (tab-bar-mode)
  
  ;; make tab bar include global information
  (dolist (var '(tab-bar-format-align-right tab-bar-format-global))
    (add-to-list 'tab-bar-format var t))
  
  (display-battery-mode)
  (display-time-mode)
  (setq display-time-day-and-date t
        display-time-load-average-threshold 100.0)) ; disable showing current load

(defun t/make-frame-small ()
  (interactive)
  (set-frame-size nil 90 60))

;; toggles
(let ((m t/leader-toggle-map))
  (define-key m "m" #'toggle-frame-maximized)
  (define-key m "f" #'toggle-frame-fullscreen)
  (define-key m "T" #'toggle-frame-tab-bar)
  (define-key m "w" #'window-toggle-side-windows)
  (define-key m "W" #'t/make-frame-small))

;;;;; Modeline

;; remove misc-info
(setq-default mode-line-format
              '(""
                (eldoc-mode-line-string
                 (" " eldoc-mode-line-string " "))
                "%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-end-spaces))

;; show current column
(column-number-mode)

;; shorten minor modes list
(use-package minions
  :straight t
  :config
  (minions-mode))

;; Theme Emacs if directed by user
(if (member 'theming t/modules)
    (load (expand-file-name "init-theming.el" user-emacs-directory)))

;;;;; Completion

;; built in completion
(use-package icomplete
  :disabled
  :init
  (defvar t/icomplete-page-scroll-margin 1
    "Number of entries that will stay on screen from last page during scroll.")
  (defun t/icomplete-next-page (arg)
    "Cycle completions forward by ARG pages."
    (interactive "p")
    (let ((height (- max-mini-window-height 2)))
      (dotimes (n (- (* height
                        arg)
                     t/icomplete-page-scroll-margin))
        (icomplete-forward-completions))))
  (defun t/icomplete-prev-page (arg)
    "Cycle completions backward by ARG pages."
    (interactive "p")
    (let ((height (- max-mini-window-height 2)))
      (dotimes (n (- (* height
                        arg)
                     t/icomplete-page-scroll-margin))
        (icomplete-backward-completions))))
  :config
  (setq icomplete-scroll t
        icomplete-show-matches-on-no-input t
        icomplete-tidy-shadowed-file-names t
        icomplete-compute-delay 0)
  (if (>= emacs-major-version 28)
      (icomplete-vertical-mode))
  (general-def
    :keymaps 'icomplete-minibuffer-map
    :states '(emacs normal insert)
    "C-n" #'icomplete-forward-completions
    "C-p" #'icomplete-backward-completions
    "C-v" #'t/icomplete-next-page
    "M-v" #'t/icomplete-prev-page
    ;; Switch `C-j' and `RET'.  RET executes candidate at point, while `C-j'
    ;; immediately sends the current input exclusively
    ;; "C-j" #'icomplete-ret
    "C-j" #'minibuffer-complete-and-exit
    "RET" #'icomplete-fido-ret
    ;; "RET" #'icomplete-fido-ret
    "C-." nil)
  :hook (emacs-startup . icomplete-mode))

;; vertical completion candidates like ivy/helm
(use-package icomplete-vertical
  :disabled
  :if (<= emacs-major-version 27)
  :straight t
  :after icomplete
  :defer t
  :config
  (setq icomplete-vertical-prospects-height 15)
  (icomplete-vertical-mode))

;; alternate completion
(use-package vertico
  :straight t
  :config
  (vertico-mode))

;; sane fuzzy matching
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults '((file (styles . (basic substring)))))
  (savehist-mode))

;; candidate annotations
(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

;; completing extensions of regular commands
(use-package consult
  :straight t
  :defer t

  :init
  (general-def
    [remap repeat-complex-command] #'consult-complex-command
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
    "C-x r b" #'consult-bookmark
    "M-#" #'consult-register-load
    "M-'"  #'consult-register-store
    "C-M-#"  #'consult-register
    "M-y" #'consult-yank-pop
    "<help> a" #'consult-apropos)

  (t/leader-def "g" '(:keymap goto-map :which-key "goto"))
  (general-def goto-map
    "e" #'consult-compile-error
    "f" #'consult-flycheck
    "g" #'consult-goto-line
    "o" #'consult-outline
    "m" #'consult-mark
    "k" #'consult-global-mark
    "i" #'consult-imenu
    "I" #'consult-imenu-multi)

  (t/leader-def "s" '(:keymap search-map :which-key "search"))
  (general-def search-map
    "f" #'consult-find
    "F" #'consult-locate
    "g" #'consult-grep
    "G" #'consult-git-grep
    "r" #'consult-ripgrep
    "l" #'consult-line
    "L" #'consult-line-multi
    "m" #'consult-multi-occur
    "k" #'consult-keep-lines
    "u" #'consult-focus-lines))

;; select from useful directories during minibuffer commands
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir))
  :config
  (setq consult-dir-project-list-function 'nil))

;; contextual actions on the thing at point
(use-package embark
  :straight t
  :config
  (general-def
    :states '(normal insert visual emacs)
    :keymaps '(global)
    "C-." #'embark-act
    "C-h B" #'embark-bindings))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; dropdown completion (explicitly call with M-TAB)
(use-package corfu
  :disabled
  :straight t
  :hook
  ((prog-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :config
  (setq corfu-scroll-margin 0)
  ;; Fall back to consult completion in terminals
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (display-graphic-p)
                     #'corfu--completion-in-region
                   #'consult-completion-in-region)
                 args))))

(use-package company
  :straight t

  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-global-modes '(not org-mode))

  :hook
  (prog-mode . company-mode)

  :config
  ;; remove dabbrev
  (setq company-backends (delete 'company-dabbrev company-backends))

  (general-def
    :keymaps 'company-active-map
    :states '(emacs insert motion)
    "TAB" #'company-complete-common-or-cycle ; not working?
    "<backtab>" (lambda ()
                  (interactive)
                  (company-complete-common-or-cycle -1))))

;;;;; Documentation

;; enhanced C-h help system
(use-package helpful
  :straight t
  :defer t
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
  (general-def t/leader-emacs-map "k" #'which-key-show-top-level))

;;;; Modes --- Filetype-specific settings

;; HTML browser
(use-package eww
  :defer t
  :hook (eww-mode . (lambda () (setq cursor-type '(bar . 2)))))

;; View info pages
(use-package info
  :config
  ;; Preserve common Info keymaps
  (general-def
    :keymaps 'Info-mode-map
    :states '(normal)
    "m" #'Info-menu
    "n" #'Info-next
    "p" #'Info-prev
    "[" #'Info-backward-node
    "]" #'Info-forward-node))

;;;;; Prose Modes

;; enhanced pdf viewing
(use-package pdf-tools
  :straight t
  :defer t
  :config
  (pdf-tools-install 'no-query)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; visual writing environment
(use-package olivetti
  :straight t
  :defer t
  
  :init
  (define-key t/leader-toggle-map "o" #'olivetti-mode)
  
  :config
  (setq-default olivetti-body-width (+ fill-column 5)))

;; org-mode settings
(load-file (expand-file-name "init-org.el" user-emacs-directory))

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

;; haskell
(use-package haskell-mode
  :straight t)

;;;; External Processes

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; help emacs find executables
(dolist (path '("/Library/TeX/texbin/" "/usr/local/bin"))
  (add-to-list 'exec-path path))

(with-eval-after-load 's
  (setenv "PKG_CONFIG_PATH"
          "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  (setenv "PATH"
          (s-join ":" exec-path)))

;;;; Code

;; snippets and templates
(use-package yasnippet
  :straight t

  :config
  (yas-global-mode))

;; packaged snippets
(use-package yasnippet-snippets
  :straight t)

;; error checking
(use-package flycheck
  :straight t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; emacs lisp
(use-package flycheck-package
  :straight t
  :defer t
  :after flycheck)

;; xref-backend that supports many languages
(use-package dumb-jump
  :straight t
  :commands t/maybe-lsp)

;; colorize colors specified in code
(use-package rainbow-mode
  :straight t
  :defer t

  :init
  (define-key t/leader-toggle-map "r" #'rainbow-mode))

;;;;; Format

;; keep things aligned
(use-package aggressive-indent-mode
  :straight t
  :hook
  (prog-mode . global-aggressive-indent-mode))

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

;; Load LSP config if directed by user
(if (member 'lsp t/modules)
    (load (expand-file-name "init-lsp.el" user-emacs-directory)))

;; Load Tree Sitter config if directed by user
(if (member 'tree-sitter t/modules)
    (load (expand-file-name "init-tree-sitter.el" user-emacs-directory)))

;; Load sly config if directed by user
(if (member 'sly t/modules)
    (load (expand-file-name "init-sly.el" user-emacs-directory)))

;;;; Apps

;; terminal emulator
(use-package vterm
  :straight t
  :defer t
  :commands vterm

  :config
  (general-def t/leader-open-map "t" #'vterm))

(use-package eshell
  :defer t
  :init
  (setq eshell-banner-message "")

  (defun t/eshell-del-char-or-exit (arg)
    "Mimic C-d behavior of traditional shells.
If point is at the end of the eshell prompt and the prompt is
empty, bury the buffer.  Otherwise, delete ARG characters
forward."
    (interactive "p")
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line (buffer-substring bol eol)))
      (if (and (looking-back eshell-prompt-regexp)
               (= (point)
                  eol))
          (bury-buffer)
        (delete-char arg))))

  (general-def t/leader-open-map "e" #'eshell)

  :hook
  ;; consult-outline jumps between previous prompts in shell buffer
  (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)))

  :config
  (require 'esh-mode)
  (define-key eshell-mode-map (kbd "C-d") #'t/eshell-del-char-or-exit))

;; Atom/RSS feed viewer
(use-package elfeed
  :straight t
  :config
  (general-def t/leader-open-map "f" #'elfeed)
  (setq elfeed-feeds
        '(("https://archlinux.org/feeds/news/" linux arch)
          ("https://xkcd.com/rss.xml" content))))

;; Load emms config if directed by user
(if (member 'emms t/modules)
    (load (expand-file-name "init-emms.el" user-emacs-directory)))

;;;;; Email

;; view email; requires installing mu (maildir-utils on some distros) first
;; Settings necessary for sending mail are located in local.el
;; Copy TEMPLATE-local.el to local.el and fill in information
(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e" ; macOS-specific?
  :defer t

  :config
  (setq
   mail-user-agent 'mu4e-user-agent
   mu4e-get-mail-command "mbsync gmail"
   ;; Gmail does this itself
   mu4e-sent-messages-behavior 'delete)
  (general-def t/leader-open-map "m" #'mu4e))

;;;; Reenable automatic mode handling according to file

;; reminds user to keep good posture
(use-package posture
  :config
  (general-def t/leader-toggle-map "p" #'toggle-posture-reminder))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist t/file-name-handler-alist)
	    (setq gc-cons-threshold gc-cons-threshold-original)))

;;; init.el ends here

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
