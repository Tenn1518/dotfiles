;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tanzeem Nazmee"
      user-mail-address "tanzeem.nazmee@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
 (setq doom-font (font-spec :family "Iosevka" :size 11.0 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "Source Sans 3" :size 11.0)
       doom-big-font (font-spec :family "Iosevka" :size 14.0 :weight 'bold))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; org config
(use-package org
  :init
  (setq! org-hide-emphasis-markers t
         org-startup-indented t
         org-startup-with-inline-images t
         org-startup-with-latex-preview t
         org-archive-default-command #'org-archive-to-archive-sibling

         org-directory "~/Dropbox/Notes/"
         org-journal-dir (concat org-directory "journal/")
         org-agenda-files (list org-directory org-journal-dir)
         tn/tasks-file (concat org-directory "tasks.org")
         tn/project-file (concat org-directory "projects.org")
         tn/school-file (concat org-directory "school.org")

         org-todo-keywords
         '((sequence "TODO(t)"
                     "NEXT(n)"
                     "|"
                     "DONE(d)"
                     "CANCELLED(c!)")
           (type "IN-PROGRESS(i)"
                 "WAITING(w)"
                 "|"
                 "DONE(d)"
                 "CANCELLED(c)")
           (sequence "UNREAD(u)"
                     "READ(r)")
           (sequence "EVENT(e)"
                     "OVER(o)"))
         org-tag-persistent-alist
         '(("note")
           ("event")
           ("task")
           ("school"))
         org-capture-templates
         '(("t" "Personal todo" entry
            (file+headline tn/tasks-file "Inbox")
            "* TODO %? :task:" :prepend t)
           ("n" "Personal note" entry
            (file+headline tn/tasks-file "Inbox")
            "* %u %? :note:" :prepend t)
           ("e" "Upcoming event" entry
            (file+headline tn/tasks-file "Inbox")
            "* EVENT %? :event:\nSCHEDULED: %^{Event Date}t")

           ("p" "Templates for projects")
           ("pt" "Project-local todo" entry
            (file+headline tn/project-file "PROJECTS")
            "* TODO %?\n%i\n%a" :prepend t)
           ("pn" "Project-local notes" entry
            (file+headline tn/project-file "Inbox")
            "* %U %?\n%i\n%a" :prepend t)
           ("pc" "Project-local changelog" entry
            (file+headline tn/project-file "Unreleased")
            "* %U %?\n%i\n%a" :prepend t)

           ("s" "Templates for school")
           ("st" "Class-related todo" entry
            (file+headline tn/school-file "Inbox")
            "* TODO %^{Title} :%^{Type|hw|study}:\nDEADLINE: %^{Deadline}t\n+ [[%x][Turn-In]]\n+ %?"
            :prepend t)
           ("sn" "Class-related notes" entry
            (file+headline tn/school-file "Inbox")
            "* %U %^{Title} :%^{Type|notes|class-info}:\n** %^{First heading}\n+ %?"
            :prepend t)

           ("x" "Class-related todo" entry
            (file+headline tn/school-file "Inbox")
            "* TODO %:description%? :%^{Type|hw|study}:\nDEADLINE: %^{Deadline}t\n+ [[%:link][%:description]]\n"
            :prepend t))

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
  :config
  (set-face-attribute 'org-level-1 nil :height 160)
  (set-face-attribute 'org-level-2 nil :height 135)
  (set-face-attribute 'org-level-3 nil :height 130)
  (set-popup-rule! "^CAPTURE" :size 0.4 :side 'bottom :select t)
  (setq org-preview-latex-image-directory (concat "~/.cache/emacs/" "ltximg")
        org-format-latex-options '( :foreground "White"
                                    :background "#282a36"
                                    :scale 1.0
                                    :html-foreground "Black"
                                    :html-background "Transparent"
                                    :html-scale 1.0
                                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

;; better org-agenda
;; (use-package org-super-agenda)

(use-package! org-roam
  :init
  (setq! org-roam-directory (concat org-directory "roam")
         org-roam-capture-templates '(( "d"
                                        "default"
                                        plain #'org-roam-capture--get-point
                                        "%?"
                                        :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                        :head "#+title: ${title}\n#+STARTUP: showeverything"
                                        :unnarrowed t)
                                      ( "m"
                                        "math note"
                                        plain #'org-roam-capture--get-point
                                        "%?"
                                        :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                        :head "#+title: ${title}\n#+roam_tags: math\n#+STARTUP: latexpreview showeverything"
                                        :unnarrowed t)
                                      ( "s"
                                        "school note"
                                        plain #'org-roam-capture--get-point
                                        "%?"
                                        :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                        :head "#+title: ${title}\n#+roam_tags: %^{engl|fsci|pltw|art|gym|phys|psych}\n#+STARTUP: showeverything"
                                        :unnarrowed t))))

(use-package! org-roam-server
  :init
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  :hook (org-roam-mode . org-roam-server-mode))

(use-package org-super-agenda
  :disabled
  :config
  (let ((org-super-agenda-groups
       '(
         ( :name "Homework Due"
           :time-grid t
           :and (:todo "TODO" :tag "hw")
           :date today)
         ( :name "Homework"
           :and (:todo "TODO" :tag "hw")
           :discard (:not (:date today)))
         )))
  (org-agenda nil "a")))

(setq! evil-collection-setup-minibuffer t)

(use-package! ligature
    :config
    (ligature-set-ligatures 'prog-mode
                            '("<->" "<-->" "<--->" "<---->" "<----->" "<=>"
                              "<==>" "<===>" "<====>" "<=====>" "<**>" "<***>"
                              "<****>" "<*****>" "<!--" "<!---" "<$" "<$>"
                              "$>" "<." "<.>" ".>" "<*" "<*>" "*>" "<\\"
                              "<\\>" "\\>" "</" "</>" "/>" "<^" "<^>" "^>"
                              "<&" "<&>" "&>" "<%" "<%>" "%>" "<@" "<@>" "@>"
                              "<#" "<#>" "#>" "<+" "<+>" "+>" "<-" "<--"
                              "<---" "<----" "<-----" "<------" "<->" "->"
                              "<!" "<!>" "!>" "<?" "<?>" "?>" "<|" "<|>" "|>"
                              "<:" "<:>" ":>" "::" ":::" "::::" "->" "->-"
                              "->--" "->>" "->>-" "->>--" "->>>" "->>>-"
                              "->>>--" "-->" "--->" "---->" "----->" "------>"
                              "-->-" "-->--" "-->>" "-->>-" "-->>--" "-->>>"
                              "-->>>-" "-->>>--" ">-" ">--" ">>-" ">>--"
                              ">>>-" ">>>--" "=/=" "=!=" "=>" "=>=" "=>=="
                              "=>>" "=>>=" "=>>==" "=>>>" "=>>>=" "=>>>=="
                              "==>" "==>=" "==>==" "==>>" "==>>=" "==>>=="
                              "==>>>" "==>>>=" "==>>>==" ">=" ">==" ">>="
                              ">>==" ">>>=" ">>>==" "-<-" "--<-" "<<-" "-<<-"
                              "--<<-" "<<<-" "-<<<-" "--<<<-" "-<--" "--<--"
                              "<<--" "-<<--" "--<<--" "<<<--" "-<<<--"
                              "--<<<--" "-<" "--<" "-<<" "--<<" "-<<<" "--<<<"
                              "<=" "=<=" "==<=" "<<=" "=<<=" "==<<=" "<<<="
                              "=<<<=" "==<<<=" "<==" "=<==" "==<==" "<<=="
                              "=<<==" "==<<==" "<<<==" "=<<<==" "==<<<==" "=<"
                              "==<" "=<<" "==<<" "=<<<" "==<<<" ">=>" ">->"
                              ">-->" ">==>" "<=<" "<-<" "<--<" "<==<" ">>"
                              ">>>" "<<" "<<<" ":+" ":-" ":=" "+:" "-:" "=:"
                              "=^" "=+" "=-" "=*" "=/" "=%" "^=" "+=" "-="
                              "*=" "/=" "%=" "/\\" "\\/" "<>" "<+" "<+>" "+>"
                              "==" "===" "!==" "!=" "++" "+++" "|-" "-|" "<~~"
                              "<~" "~>" "~~>" "~=" "(*" "*)"))
    (global-ligature-mode t))

(use-package! which-key
  :custom
  (which-key-idle-delay 0.4)
  :config
  (which-key-setup-side-window-right-bottom))

;; mode-line
(after! doom-modeline
  (display-battery-mode)
  (doom-modeline-def-modeline 'main
    '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs battery "  ")))

(use-package! company-box
  :disabled
  :hook (company-mode . company-box-mode))

(after! helm
  (set-popup-rule! "^\\*helm" :size 0.4))

;; programmatically set SPC 0-9 to winum-select-window-
;; (cl-mapcar
;;  (lambda (num)
;;    (let* ((strnum (int-to-string num))
;;           (str (concat "Switch to workspace" strnum)))
;;      (eval `(map! :leader
;;                   :desc ,str ,(int-to-string num) ,(intern (concat "winum-select-window-" strnum))))))
;;  (number-sequence 0 9))

;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(add-load-path! "/usr/share/emacs/site-lisp/mu4e")
(set-email-account! "gmail"
                    '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
                      (mu4e-trash-folder      . "/[Gmail].Trash")
                      (mu4e-refile-folder     . "/[Gmail].All Mail")
                      (smtpmail-smtp-server "smtp.gmail.com")
                      (smtpmail-smtp-service 25)
                      (mu4e-compose-signature . "---\nTanzeem Nazmee"))
                    t)

(unless (featurep! :tn exwm)
  (mouse-avoidance-mode 'banish))

;; fix for weird problem that prevents helm usage on startup
(setq browse-url-mosaic-program nil)
