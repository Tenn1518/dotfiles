;;; init-org.el --- Org-mode settings                -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Tenn1518

;; Author:  Tenn1518

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package org
  :straight t
  :defer t

  :init
  (setq
   ;; relevant paths
   org-directory "~/Documents/Notes/"
   t/daily-file (expand-file-name "daily.org" org-directory)
   ;; appearance
   org-indent-indentation-per-level 1
   org-startup-indented nil
   org-startup-with-inline-images t
   org-startup-with-latex-preview nil   ; slows org utils that open many buffers
   org-fontify-whole-heading-line t
   org-pretty-entities t
   ;; behavior
   org-archive-default-command #'org-archive-to-archive-sibling
   org-id-track-globally t
   org-imenu-depth 7
   org-use-speed-commands t
   org-catch-invisible-edits 'show-and-error
   org-list-demote-modify-bullet '(("+" . "-")
                                   ("-" . "+"))
   org-image-actual-width nil
   ;; keybind
   org-support-shift-select nil
   org-ctrl-k-protect-subtree t
   org-special-ctrl-a/e 'reversed
   org-M-RET-may-split-line '((default . nil)))
  
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

  ;; Finds project.org in current project directory
  (defun t/org-capture-project-dir ()
    (let* ((project (project-current nil default-directory))
           (root (if project
                     (project-root project)
                   (error "Not in a project"))))
      (expand-file-name "project.org" root)))
  
  ;; tags and captures
  (setq org-tag-persistent-alist
        '(("note")
          ("event")
          ("task")
          ("school")))

  (setq org-capture-templates
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

  ;; org shortcuts
  (t/leader-def
    "c" #'org-capture
    "l" #'org-store-link
    "n f" #'t/edit-org-dir)

  :config
  (setq ;; appearance settings
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-ellipsis " ▼ "
   ;; latex settings
   org-preview-latex-image-directory "~/.cache/emacs/ltximg/"
   org-format-latex-options '( :foreground "Black"
                               :background "White"
                               :scale 1.5
                               :html-foreground "Black"
                               :html-background "Transparent"
                               :html-scale 1.0
                               :matchers ("begin"
                                          "$1"
                                          "$"
                                          "$$"
                                          "\\(" "\\["))
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
                                 "OVER(o)"))
   ;; Overlay dates with American format
   org-display-custom-times t
   org-time-stamp-custom-formats
   '("<%m/%d/%y %a>" . "<%m/%d/%y %a %l:%M %p>"))

  ;; keybindings
  (general-def 'normal 'org-capture-mode-map
    "ZZ" #'org-capture-finalize
    "ZQ" #'org-capture-kill)
  ;; edit/nav bindings in org-mode (emacs)
  (general-def org-mode-map
    "C-s-f" #'org-metaright
    "C-s-b" #'org-metaleft
    "C-s-p" #'org-metaup
    "C-s-n" #'org-metadown
    "C-S-s-f" #'org-shiftmetaright
    "C-S-s-b" #'org-shiftmetaleft
    "C-S-s-p" #'org-shiftmetaup
    "C-S-s-n" #'org-shiftmetadown
    ;; Make item at point heading
    "C-s-h" #'org-toggle-heading
    ;; surround region or word at point in emphasis markers
    "C-s-e" #'t/org-emphasize
    :states 'normal
    "zk" #'org-previous-visible-heading
    "zj" #'org-next-visible-heading)
  ;; Search headings
  (t/leader-def 'org-mode-map
    :infix "g"
    "o" #'consult-org-heading)

  (define-skeleton t/org-note
    "Boilerplate for an org-mode file."
    "Title of note: "
    "#+TITLE: " str
    ("Author: "
     "\n#+AUTHOR: " str)
    "\n#+OPTIONS: num:nil"
    "\n\n" _)

  :hook (org-mode . auto-fill-mode))

(use-package org-agenda
  :defer t
  :after org
  
  :init
  (defun t/org-agenda-view ()
    "Directly open org-agenda to the default agenda view."
    (interactive)
    (org-agenda nil "a"))
  (define-key t/leader-notes-map "a" #'org-agenda)

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

  :bind (("<f6>" . #'t/org-agenda-view)))

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

;; vim keybindings for org
(use-package evil-org
  :straight t
  :defer t
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(insert textobjects additional calendar)) ; navigation 
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; smarter variable pitch
(use-package org-variable-pitch
  :straight t
  :after org
  :config
  (set-face-attribute 'org-variable-pitch-fixed-face nil :height 140)
  :bind
  (:map org-mode-map
        ("C-s-v" . org-variable-pitch-minor-mode)))

(use-package org-bullets
  :disabled
  :straight t

  :hook (org-mode . org-bullets-mode)
  
  :config
  (setq org-bullets-bullet-list '(" "))) ; set bullets invisible

;; rendered org tables
(use-package org-pretty-table
  :straight (org-pretty-table
             :type git
             :host github
             :repo "Fuco1/org-pretty-table")

  :hook (org-mode . org-pretty-table-mode))

;; drag and drop images to org-mode
(use-package org-download
  :straight t

  :config
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir (concat org-directory "img/"))

  :hook (dired-mode . org-download-enable))

;; journal
(use-package org-journal
  :straight t
  :after org
  :defer t

  :init
  (setq org-journal-file-format "%Y%m%d.org")
  (define-key t/leader-notes-map "j" #'org-journal-new-entry)

  :config
  (setq org-journal-dir (expand-file-name (concat org-directory "journal/"))
        ;; don't open new window
        org-journal-find-file #'find-file)
  (push org-journal-dir org-agenda-files))

;; atomic note taker
(use-package org-roam
  :disabled
  :straight t
  :defer t

  :init
  (setq org-roam-directory (concat org-directory "roam")
        org-roam-v2-ack t) ;; silence upgrade warning
  ;; (t/notes-def
  ;;   :infix "r"
  ;;   "f" #'org-roam-node-find
  ;;   "i" #'org-roam-node-insert
  ;;   "I" #'org-id-get-create
  ;;   "n" #'org-roam-capture
  ;;   "r" #'org-roam-buffer-toggle)

  :config
  (setq org-roam-capture-templates
        ;; TODO: "d/m" templates aren't updated for roam-v2
        '(( "d"
            "Default"
            plain #'org-roam-capture--get-point
            "%?"
            :file-name "%<%Y%m%d%H%M%S>-${slug}"
            :head "#+title: ${title}\n#+STARTUP: showeverything"
            :unnarrowed t)
          ( "m"
            "Math note"
            plain #'org-roam-capture--get-point
            "%?"
            :file-name "%<%Y%m%d%H%M%S>-${slug}"
            :head "#+title: ${title}\n#+roam_tags: math\n#+STARTUP: latexpreview showeverything"
            :unnarrowed t)
          ( "p" "Notes with associated document" plain
            "%?"
            :if-new
            (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       ":PROPERTIES:\n:NOTER_DOCUMENT: %F\n:END:\n#+title: ${title}\n#+STARTUP: latexpreview showeverything")
            :unnarrowed t)))

  (org-roam-db-autosync-mode))

;; browser page for viewing/navigating org-roam notes
(use-package org-roam-ui
  :disabled
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam

  :init
  (t/notes-def "ru" #'org-roam-ui-mode)

  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; search plaintext files
(use-package deft
  :straight t
  :init
  (defun t/deft-kill-existing-buffer ()
    "Kill any existing deft buffer.
Return nil if no deft buffer exists."
    (require 'deft)
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
    (require 'org-roam)
    (t/deft-in-dir org-roam-directory))

  (let ((m t/leader-notes-map))
    (define-key m "s" #'deft)
    (define-key m "S" #'t/deft-in-dir))

  :bind
  (("<f8>" . deft)                       ; Display existing or new deft buffer
   ("C-<f8>" . t/deft-reset)             ; Open new deft buffer
   ("M-<f8>" . t/deft-in-roam-dir)       ; Open deft buffer limited to org-roam notes
   :map deft-mode-map
   ("C-w" . deft-filter-decrement-word)) ; Kill filter string backward
  
  :config
  ;; following parser replacement from https://githubmemory.com/repo/jrblevin/deft/issues/75
  ;; title is taken from "#+TITLE"
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
        deft-recursive t
        deft-auto-save-interval 0)      ; disable auto-saving

  ;; ensure :PROPERTIES: and lowercase #+props are not shown in note summaries
  (setq deft-strip-summary-regexp "\\(^:.*:.*\\|[\n	]\\|^#\\+[[:alpha:]_]+:.*$\\)"))

;; cite sources from .bib files
(use-package org-ref
  :disabled
  :straight t
  :defer t
  :config
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

  :init
  (define-key t/leader-notes-map "n" #'org-noter))

;; flashcards
(use-package org-drill
  :straight t
  :defer t)

;; enhanced calendar view
(use-package calfw
  :straight t)
(use-package calfw-org
  :straight t
  :config
  (define-key t/leader-notes-map "c" #'cfw:open-org-calendar))


(provide 'init-org)
;;; init-org.el ends here
