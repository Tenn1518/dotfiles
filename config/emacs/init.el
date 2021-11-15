;;; init.el --- my emacs config
;; 5-3-2020

;;; Commentary:

;; A personal Emacs config.

;; The config is split into multiple sections:
;; 1. Package Management
;; 2. Appearance
;; 3. Emacs Lisp settings
;; 4. Navigation/Editing
;; 5. Completion
;; 6. Programming
;; 7. Org-mode
;; 8. Miscellaneous

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

;; set rules for displaying buffers
(setq display-buffer-alist
      '(("\\`\\*helm.*?\\*\\'" 
         (display-buffer-at-bottom)
         (window-height . .35))
        ("^\\*vterm.*\\*$"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . -1)
         (window-width . .3)
         (window-height . .3)
         (window-parameters . ((mode-line-format . none))))
        ("(\\*Messages\\*|Output\\*$)"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (height . .3))
        ("\\(^\\*helpful\\|\\*Help\\*\\)"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (window-height . .3))
        ("\\(^magit:\\|\\*info\\*\\|NEWS\\)"
         (display-buffer-reuse-window
          display-buffer-use-some-window
          display-buffer-in-direction)
         (direction . right)
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

;; display line numbers in programming buffers only
(defun t/line-number ()
  (unless (minibufferp)
    (display-line-numbers-mode)))
(add-hook 'prog-mode-hook 't/line-number)

;; scroll long lines individually like nano
(setq auto-hscroll-mode 'current-line)

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

;; you're not immune to vanity
(use-package all-the-icons
  :straight t)

;; spacemacs modeline
(use-package spaceline
  :straight t
  :config
  (require 'spaceline-config)
  (setq powerline-height 16
        powerline-default-separator 'wave)
  (spaceline-spacemacs-theme))


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
  :bind
  ("C-h k" . #'helpful-key)
  ("C-h v" . #'helpful-variable)
  ("C-h f" . #'helpful-callable))


;; Navigation/Editing

;; acceptable line length
(setq-default fill-column 80)

;; never use tabs
(setq-default indent-tabs-mode nil)

;; don't permanently delete files
(setq delete-by-moving-to-trash t)
(use-package osx-trash
  :if (eq 'system-type 'darwin)
  :straight t
  :config
  (osx-trash-setup))

;; set macOS modifier keys
(when (eq system-type 'darwin)
  ;; command is switched with control
  (setq mac-right-option-modifier 'meta
        mac-right-command-modifier 'control
        mac-command-modifier 'control
        mac-control-modifier 'super)
  ;; prevent accidental press
  (global-unset-key (kbd "s-g")))

;; point dired to correct ls binary on macOS
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; replaces unnecessary suspend command
(global-set-key (kbd "C-z") #'zap-up-to-char)

;; ibuffer for listing buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Edit emacs directory
(defun t/edit-emacs-dir ()
  "Edit file from Emacs configuration directory."
  (interactive)
  (find-file user-emacs-directory))
(global-set-key (kbd "C-c f p") #'t/edit-emacs-dir)

;; Open scratch buffer
(defun t/open-scratch ()
  (interactive)
  (pop-to-buffer "*scratch*"))
(global-set-key (kbd "C-c x") #'t/open-scratch)

;; Toggles
(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c t m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t v") #'visual-line-mode)

;; Applications
(global-set-key (kbd "C-c o c") #'calendar)

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

;; ~M-o~ to change window
(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "M-o") #'ace-window))

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

;; file explorer window
(use-package neotree
  :straight t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-mode-line-type 'none)
  (set-face-attribute 'neo-file-link-face nil :inherit 'variable-pitch)
  (set-face-attribute 'neo-dir-link-face nil :inherit 'variable-pitch)
  (add-hook 'neotree-mode-hook #'hl-line-mode)
  :bind ("C-c t t" . #'neotree-toggle))


;; Completion

(use-package helm
  :straight t
  :bind
  ("M-x" . #'helm-M-x)
  ([remap apropos] .  #'helm-apropos)
  ([remap find-library] .  #'helm-locate-library)
  ([remap bookmark-jump] . #'helm-bookmarks)
  ([remap execute-extended-command] . #'helm-M-x)
  ([remap find-file] . #'helm-find-files)
  ([remap imenu] . #'helm-semantic-or-imenu)
  ([remap locate] . #'helm-locate)
  ([remap noop-show-kill-ring] . #'helm-show-kill-ring)
  ([remap occur] . #'helm-occur)
  ([remap switch-to-buffer] . #'helm-buffers-list)
  ([remap recentf-open-files] . #'helm-recentf)
  ("C-c s y" . #'helm-show-kill-ring)
  ("C-c s f" . #'helm-find)
  ("C-c s a" . #'helm-do-grep-ag)
  :config
  (setq helm-split-window-in-side-p nil)
  (helm-mode))

;; search through buffer
(use-package helm-swoop
  :straight t
  :bind ("C-c s s" . #'helm-swoop))

;; Programming

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

;; manage projects
(use-package projectile
  :straight t
  :init
  (setq projectile-switch-project-action #'projectile-dired
        projectile-project-search-path '(("~/Projects" . 2)))
  :config
  (define-key projectile-mode-map (kbd "C-c p") projectile-command-map)
  (projectile-mode))

;; integrate with helm actions
(use-package helm-projectile
  :disabled
  :straight t
  :after (helm projectile)
  :config
  (helm-projectile-on))

;; git frontend
(use-package magit
  :straight t
  :bind (("C-x M-g" . magit-dispatch)
         ("C-c o g" . magit-status)))

;; automatic formatting of program buffers on save
(use-package format-all
  :commands format-all-mode
  :straight t)

;; IDE-like features through language servers
(use-package lsp-mode
  :straight t
  :hook
  ((lsp-mode . t/lsp-onsave))
  :commands lsp lsp-deferred
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c c")
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
  :hook (prog-mode . flycheck-mode))

;; highlight changes in fringe
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; highlight indentation
(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :bind ("C-c t i" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; terminal emulator
(use-package vterm
  :straight t
  :commands vterm
  :bind ("C-c o t" . vterm))


;; Org-mode

(use-package org
  :straight t
  :hook (org-mode . auto-fill-mode)
  :init
  (setq
   org-startup-with-inline-images t
   org-startup-with-latex-preview t
   org-archive-default-command #'org-archive-to-archive-sibling
   org-id-track-globally t
   ;; relevant paths
   org-directory "~/Documents/Notes/"
   org-agenda-files (list org-directory)
   t/daily-file (expand-file-name "daily.org" org-directory))
  ;; tags and captures
   (setq org-tag-persistent-alist
   '(("note")
     ("event")
     ("task")
     ("school"))
   org-capture-templates
   `(("t" "Quick todo" entry
      (file+olp+datetree t/daily-file)
      "* TODO %?\n%U"
      :empty-lines 2 :prepend t)
     ("n" "Quick note" entry
      (file+olp+datetree t/daily-file)
      "* UNREAD %? :note:\n%U"
      :empty-lines 2 :prepend t)
     ("e" "Upcoming event" entry
      (file+olp+datetree t/daily-file)
      "* EVENT %? :event:\nSCHEDULED: %^{Event Date}t"
      :empty-lines 2 :prepend t)
     ;; entries for school file
     ("s" "Templates for school")
     ("st" "Class-related todo" entry
      (file+olp+datetree t/daily-file)
      ,(concat "* TODO %^{Title} :hw:\n"
               "DEADLINE:%^{Deadline}t\n\n"
               "+ [[%?][Turn-In]]")
      :empty-lines 2)
     ("sn" "Class-related notes" entry
      (file+olp+datetree t/daily-file)
      "* %^{Title} :%^{Type|notes|class-info}:\n%U\n\n+ %?"
      :empty-lines 2)))
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
  ;; open file from org-directory
  (defun t/edit-org-dir ()
    (interactive)
    (counsel-find-file org-directory))
  ;; C-c global bindings
  (global-set-key (kbd "C-c f n") #'t/edit-org-dir)
  (global-set-key (kbd "C-c X") #'org-capture)
  (global-set-key (kbd "C-c n a") #'org-agenda)
  (global-set-key (kbd "C-c n l") #'org-store-link)
  ;; edit/nav bindings in org-mode
  (define-key org-mode-map (kbd "C-c .") #'counsel-org-goto)
  (define-key org-mode-map (kbd "M-{") #'backward-paragraph)
  (define-key org-mode-map (kbd "M-}") #'forward-paragraph))

(use-package ox-latex
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
  :straight t)

;; journal
(use-package org-journal
  :straight t
  :bind ("C-c n j" . org-journal-new-entry)
  :after org
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
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-export-latex-format-toc-function 'org-export-latex-no-toc
        org-ref-get-pdf-filename-function
        (lambda (key) (car (bibtex-completion-find-pdf key)))
        ;; For pdf export engines.
        org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
        ;; I use orb to link org-ref, helm-bibtex and org-noter together (see below for more on org-noter and orb).
        ;;org-ref-notes-function 'orb-edit-notes)
        org-ref-default-bibliography "~/Zotero/zotero/zotero.bib"
        org-ref-default-citation-link "citep"))
(use-package helm-bibtex
  :straight t
  :config
  (setq bibtex-completion-bibliography org-ref-default-bibliography))

;; taking notes from a document
(use-package org-noter
  :straight t
  :config
  (global-set-key (kbd "C-c n n") #'org-noter))

;; Miscellaneous

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

;; reminds user to keep good posture
(use-package posture
  :config
  (global-set-key (kbd "C-c t p") #'toggle-posture-reminder))

;; reenable garbage collection when idle
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (makunbound 'gc-cons-threshold-original)))

;;; init.el ends here
