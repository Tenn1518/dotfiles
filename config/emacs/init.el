;;; init.el --- my emacs config
;; 5-3-2020

;;; Commentary:

;; A personal Emacs config.

;;; Code:

;; disable garbage collection until post-init
(setq gc-cons-threshold-original gc-cons-threshold
      gc-cons-threshold (* 1024 1024 100))

;; Package management

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

(straight-use-package 'use-package)


;; Clean up emacs folder

(use-package no-littering
  :straight t)


;; Appearance

;; font settings
(add-to-list 'default-frame-alist '(font . "Menlo-12"))
(set-face-attribute 'default nil :family "Menlo" :height 120 :weight 'normal)

;; ensure buffer titles are unique
(use-package uniquify
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

;; display line numbers in programming buffers
(defun display-line-numbers--turn-on ()
  "Turn on line numbers if major mode is programming-related."
  (unless (or (minibufferp)
              ;;(member major-mode display-line-numbers-exempt-modes)
	      (not (derived-mode-p 'prog-mode)))
    (display-line-numbers-mode)))
(global-display-line-numbers-mode)

;; scroll long lines individually
(setq auto-hscroll-mode 'current-line)

;; modeline
(setq-default mode-line-format
	    '(""
	      "%e"
	      mode-line-front-space
	      mode-line-mule-info
	      mode-line-client
	      mode-line-modified
	      mode-line-remote
	      mode-line-frame-indication
	      "   "
	      mode-line-buffer-identification
	      " %p %l:%c "
	      ;; right-align
	      (:eval (propertize
                      " " 'display
                      `((space :align-to (- (+ right right-fringe right-margin)
                                            ,(+ 3 (string-width mode-name)))))))
	      "[%m]"))

;; macOS titlebar
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :straight t
  :config
  (ns-auto-titlebar-mode))

;; theme
(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-operandi t))


;; Emacs Lisp settings

;; add my folder of lisp programs from dotfiles
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Reenable commands
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; enhanced C-h help system
(use-package helpful
  :straight t
  :config
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))


;; Navigation/Editing

;; acceptable line length
(setq-default fill-column 80)

;; set macOS modifier keys
;; command is switched with control
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'meta
	mac-right-command-modifier 'control
	mac-command-modifier 'control
	mac-control-modifier 'super))

;; neuter annoying escape key
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; replaces unnecessary suspend command
(global-set-key (kbd "C-z") #'zap-up-to-char)
;; ibuffer for listing buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; Edit emacs directory
(defun t/edit-emacs-dir ()
  "Edit file from Emacs configuration directory."
  (interactive)
  (counsel-find-file user-emacs-directory))
(global-set-key (kbd "C-c f p") #'t/edit-emacs-dir)
;; Open scratch buffer
(defun t/open-scratch ()
  (interactive)
  (pop-to-buffer "*scratch*"))
(global-set-key (kbd "C-c x") #'t/open-scratch)
;; Toggles
(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c t f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t v") #'visual-line-mode)

;; expedites copying from org-mode to Word for essays
(defun t/make-region-pastable ()
  "Save contents of the region to the kill ring for pasting into
a separate program.  The copy is altered to remove extraneous
newlines and double spaces."
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
(global-set-key (kbd "C-c p") #'t/make-region-pastable)

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
  (which-key-setup-side-window-right-bottom))

;; replace annoying splits with popups
(use-package popwin
  :straight t
  :config
  (popwin-mode 1)
  (push '(helpful-mode :height 20 :stick t)
	popwin:special-display-config)
  (push '(vterm-mode :height 20
		     :stick t
		     :dedicated t)
	popwin:special-display-config))


;; Completion

(use-package ivy
  :straight t
  :config
  (ivy-mode)
  (setq
    ;; arbitrary user input creates new entry
    ;; "C-M-j" also does this
    ivy-use-selectable-prompt t
    ivy-use-virtual-buffers t
    ivy-count-format "(%d/%d)"
    ivy-height 15
    ;; control search style per command
    ivy-re-builders-alist
    '((counsel-M-x . ivy--regex-fuzzy)
      (counsel-find-file . ivy--regex-fuzzy)
      (t . ivy--regex-plus))))

;; more informative completion candidates
(use-package ivy-rich
  :straight t)

;; enhance common emacs commands
(use-package counsel
  :straight t
  :after (ivy)
  :config
  (global-set-key (kbd "C-h b") #'counsel-descbinds)
  (global-set-key (kbd "C-h f") #'counsel-describe-function)
  (global-set-key (kbd "C-h v") #'counsel-describe-variable)
  (global-set-key (kbd "C-h a") #'counsel-apropos)
  (global-set-key (kbd "C-x C-f") #'counsel-find-file)
  (global-set-key (kbd "C-x r b") #'counsel-bookmark)
  (global-set-key (kbd "C-c c i") #'counsel-imenu)
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "C-c y") #'counsel-yank-pop)
  (global-set-key (kbd "C-c s") #'counsel-grep-or-swiper))

;; enhanced isearch alternative with ivy
(use-package swiper
  :straight t
  :after (ivy counsel))

;; enhanced M-x
(use-package amx
  :straight t
  :config
  (amx-mode))


;; Programming

;; allow subprocesses more data at once (lsp)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; help emacs find executables
(add-to-list 'exec-path "/Library/TeX/texbin/")

;; highlight and auto-create matching parentheses
(show-paren-mode 1)
(electric-pair-mode)

;; don't fully display long lines
(setq-default truncate-lines t)

;; display region when active
(transient-mark-mode)

;; automatic formatting of program buffers on save
(use-package format-all
  :straight t)

;; IDE-like features
(use-package lsp-mode
  :straight t
  :init
  :hook
  ((lsp-mode . onsave))
  :commands lsp lsp-deferred
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  (defvar t/lsp-enabled-modes
    '(python-mode c++-mode)
    "List of major modes which LSP should activate on start.")
  (defun t/lsp-or-format-all ()
    "Enable lsp-mode if the current major mode is included in
t/lsp-enabled-modes. Otherwise, enable format-all-mode."
    (cond ((member major-mode t/lsp-enabled-modes)
	   (lsp-deferred))
	  ((derived-mode-p 'prog-mode) (format-all-mode t))))
  (defun t/lsp-onsave ()
    "Configure LSP to automatically format all code on save in
lsp-enabled buffers."
    (lsp-enable-which-key-integration)
    (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
    (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))
  (add-hook 'prog-mode-hook #'t/lsp-or-format-all))

;; error checking
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

;; git frontend
(use-package magit
  :straight t)

;; highlight changes in fringe
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; terminal emulator
(use-package vterm
  :straight t)


;; Org-mode

(use-package org
  :straight t
  :hook (org-mode . auto-fill-mode)
  :init
  (setq
   org-startup-with-inline-images t
   org-startup-with-latex-preview t
   org-archive-default-command #'org-archive-to-archive-sibling
   ;; relevant paths
   org-directory "~/Documents/Notes/"
   org-agenda-files (list org-directory)
   t/daily-file (expand-file-name "daily.org" org-directory)
   tn/tasks-file (concat org-directory "tasks.org")
   tn/project-file (concat org-directory "projects.org")
   tn/school-file (concat org-directory "school.org"))
  ;; tags and captures
   (setq org-tag-persistent-alist
   '(("note")
     ("event")
     ("task")
     ("school"))
   org-capture-templates
   ;; entries for personal tasks file (consider removing)
   '(("t" "Personal todo" entry
      (file+olp+datetree t/daily-file)
      "* TODO %? :task:"
      :empty-lines 1 :prepend t)
     ("n" "Personal note" entry
      (file+olp+datetree t/daily-file)
      "* UNREAD %u %? :note:"
      :empty-lines 1 :prepend t)
     ("e" "Upcoming event" entry
      (file+olp+datetree t/daily-file)
      "* EVENT %? :event:\nSCHEDULED: %^{Event Date}t"
      :empty-lines 1 :prepend t)
     ;; entries for school file
     ("s" "Templates for school")
     ("st" "Class-related todo" entry
      (file+olp+datetree t/daily-file)
      "* TODO %^{Title} :%^{Type|hw|study}:\nDEADLINE:%^{Deadline}t\n\n+ [[%c][Turn-In]]\n\n%?"
      :empty-lines 1)
     ("sn" "Class-related notes" entry
      (file+olp+datetree t/daily-file)
      "* %^{Title} :%^{Type|notes|class-info}:\n%U\n\n+ %?"
      :empty-lines 1)))
   ;; agenda settings
   (setq org-deadline-warning-days 3
   org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((tags-todo "task")
       (agenda ""))
      ((org-agenda-span 7)))
     ("w" "School" tags-todo "@school"
      ((org-agenda-span 5)
       (org-agenda-start-on-weekday 1)
       (org-agenda-time-grid nil)))))
  :config
  (setq ;; appearance settings
        org-hide-leading-stars nil
        org-hide-emphasis-markers t
        org-startup-indented nil
	;; latex settings
	org-preview-latex-image-directory (concat "~/.cache/emacs/" "ltximg")
        org-format-latex-options '( :foreground "White"
                                    :background "#282a36"
                                    :scale 2.0
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
  ;; open file from org-directory
  (defun t/edit-org-dir ()
    (interactive)
    (counsel-find-file org-directory))
  ;; C-c global bindings
  (global-set-key (kbd "C-c f n") #'t/edit-org-dir)
  (global-set-key (kbd "C-c X") #'org-capture)
  (global-set-key (kbd "C-c n a") #'org-agenda)
  (global-set-key (kbd "C-c n j") #'org-journal-new-entry)
  ;; edit/nav bindings in org-mode
  (define-key org-mode-map (kbd "C-c .") #'counsel-org-goto)
  (define-key org-mode-map (kbd "M-{") #'backward-paragraph)
  (define-key org-mode-map (kbd "M-}") #'forward-paragraph)
  ;; moving headings
  (define-key org-mode-map (kbd "s-b") #'org-metaleft)
  (define-key org-mode-map (kbd "s-f") #'org-metaright)
  (define-key org-mode-map (kbd "s-p") #'org-metaup)
  (define-key org-mode-map (kbd "s-n") #'org-metadown)
  (define-key org-mode-map (kbd "s-C-b") #'org-backward-heading-same-level)
  (define-key org-mode-map (kbd "s-C-f") #'org-forward-heading-same-level)
  (define-key org-mode-map (kbd "s-C-p") #'org-previous-visible-heading)
  (define-key org-mode-map (kbd "s-C-n") #'org-next-visible-heading)
  (define-key org-mode-map (kbd "s-C-u") #'outline-up-heading))

;; syntax highlighting in exported html
(use-package htmlize
  :straight t)

;; journal
(use-package org-journal
  :straight t
  :config
  (setq org-journal-dir (expand-file-name (concat org-directory "journal/"))
	;; don't open new window
	org-journal-find-file #'find-file)
  (push org-journal-dir org-agenda-files))

;; atomic note taker
(use-package org-roam
  :straight t
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
  :config
  (global-set-key (kbd "C-c n r f") #'org-roam-node-find)
  (global-set-key (kbd "C-c n r i") #'org-roam-node-insert)
  (global-set-key (kbd "C-c n r I") #'org-id-get-create)
  (global-set-key (kbd "C-c n r n") #'org-roam-capture)
  (global-set-key (kbd "C-c n r r") #'org-roam-buffer-toggle))

;; browser page for viewing/navigating org-roam notes
(use-package org-roam-ui
  :disabled
  :straight t)
(use-package websocket
  :disabled
  :after org-roam)

;; reenable garbage collection when idle
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (makunbound 'gc-cons-threshold-original)))

;;; init.el ends here
