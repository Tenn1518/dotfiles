;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
   This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers")

   ;; SPC f e R --- Reload config
   dotspacemacs-configuration-layers
   '(yaml
     sql
     csv
     auto-completion
     ;; better-defaults
     emacs-lisp
     emoji
     git
     ;; helm
     imenu-list
     (ivy :variables
          ivy-enable-advanced-buffer-information t)
     lsp
     ;; markdown
     multiple-cursors
     (org :variables
          org-enable-org-journal-support t)
     pandoc
     search-engine
     ;; +lang
     c-c++
     common-lisp
     (html :variables
           css-enable-lsp t
           html-enable-lsp t)
     (javascript :variables
                 javascript-backend 'lsp)
     (python :variables
             python-backend 'lsp)
     shell-scripts

     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     (treemacs :variables
               treemacs-use-follow-mode 'extended
               treemacs-use-all-the-icons-theme t
               treemacs-use-filewatch-mode t
               treemacs-lock-width t)

     ;; +themes
     themes-megapack
     (colors :variables )

     ;; +vim
     evil-snipe

     ;; +tools
     dap)

   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(all-the-icons-dired
                                      company-box
                                      evil-collection
                                      highlight-indent-guides
                                      mixed-pitch
                                      org-variable-pitch
                                      solaire-mode)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(indent-guide
                                    vi-tilde-fringe
                                    exec-path-from-shell)
   ;; Defines the behaviour of Spacemacs when installing packages.
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 300
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-read-process-output-max (* 1024 1024)
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives nil
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-startup-buffer-show-version t
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((projects . 7)
                                (recents . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-new-empty-buffer-major-mode nil
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-dracula
                         doom-solarized-light)
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Iosevka Medium"
                               :size 32
                               :weight normal
                               :width normal)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   dotspacemacs-emacs-command-key ":"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'right-then-bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 75

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   dotspacemacs-line-numbers '( :relative nil
                                :visual nil
                                :disabled-for-modes dired-mode
                                                    doc-view-mode
                                                    org-mode
                                                    pdf-view-mode
                                                    text-mode
                                :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names.
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%f"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil activate `snoopy-mode' which shifts your number row
   ;; to match the set of signs given in `dotspacemacs-snoopy-keyrow'
   ;; in programming modes (insert-mode only). (default nil)
   dotspacemacs-use-snoopy-mode nil

   ;; Text of shifted values from your
   ;; keyboard's number row. (default '!@#$%^&*()')
   dotspacemacs-snoopy-keyrow "!@#$%^&*()"

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq evil-want-keybinding nil
        ivy-re-builders-alist '((spacemacs/smex . ivy--regex-fuzzy)
                                (counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq default-frame-alist '((font . "Iosevka Medium")
                              (size . 160)
                              (buffer-predicate . spacemacs/useful-buffer-p)
                              (vertical-scroll-bars)))
  (setq-default evil-escape-key-sequence "jk"
                powerline-height 32
                powerline-separator 'box)

  (set-face-attribute 'fixed-pitch nil :family "Iosevka Medium")
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro")

  ;; color non-file-visiting buffers darker
  (use-package solaire-mode
    :config
    (solaire-global-mode +1))

  ;; evil everywhere
  (use-package evil-collection
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init))

  (use-package highlight-indent-guides
    :hook (prog-mode . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-method 'character))

  (use-package lsp-ui
    :custom
    (lsp-ui-doc-position 'bottom))

  (use-package org
    :custom
    (org-hide-emphasis-markers t)
    (org-startup-indented t)
    (org-startup-with-inline-images t)
    (org-startup-with-latex-preview)
    (org-format-latex-options '( :foreground default
                                 :background "#282a36"
                                 :scale 4.0
                                 :html-foreground "Black"
                                 :html-background "Transparent"
                                 :html-scale 1.0
                                 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

    (org-directory "~/Dropbox/org/")
    (org-journal-dir (concat org-directory "journal/"))
    (org-agenda-files (list org-directory org-journal-dir))
    (tn/tasks-file (concat org-directory "tasks.org"))
    (tn/project-file (concat org-directory "projects.org"))
    (tn/school-file (concat org-directory "school.org"))

    (org-todo-keywords
     '((sequence "TODO(t)"
                 "NEXT(n)"
                 "IN-PROGRESS(i)"
                 "WAITING(w)"
                 "|"
                 "DONE(d!)"
                 "CANCELLED(c!)")
       (sequence "EVENT(e)"
                 "OVER(o)")))
    (org-tag-persistent-alist
     '(("note")
       ("event")
       ("task")
       ("school")))
    (org-capture-templates
     '(("t" "Personal todo" entry
        (file+headline tn/tasks-file "Inbox")
        "* TODO %? :task:" :prepend t)
       ("n" "Personal note" entry
        (file+headline tn/tasks-file "Inbox")
        "* %u %? :note:" :prepend t)
       ("e" "Upcoming event" entry
        (file+headline tn/tasks-file "Inbox")
        "* %? :event:\nSCHEDULED: %^{Event Date}t")

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
        "* TODO %? %^{Type|hw|study}g\nDEADLINE: %^{Deadline}t" :prepend t)
       ("sn" "Class-related notes" entry
        (file+headline tn/school-file "Inbox")
        "* %U %? %^{Type|notes|class-info}g" :prepend t)
       ))
    :config
    (set-face-attribute 'org-level-1 nil :height 160)
    (set-face-attribute 'org-level-2 nil :height 135)
    (set-face-attribute 'org-level-3 nil :height 130)
    (add-hook 'org-capture-mode-hook 'evil-insert-state))

  ;; Mixed usage of variable and fixed pitch fonts
  (use-package mixed-pitch
    :hook (text-mode . (lambda ()
                         (unless (derived-mode-p 'org-mode)
                           (mixed-pitch-mode)))))

  ;; Smarter usage of variable and fixed pitch fonts for org-mode
  (use-package org-variable-pitch
    :hook (org-mode . org-variable-pitch-minor-mode))

  (use-package all-the-icons-dired
    :init
    (remove-hook 'dired-mode-hook 'treemacs-icons-dired--enable-highlight-correction)
    (remove-hook 'dired-mode-hook 'treemacs-icons-dired-mode)
    (remove-hook 'dired-mode-hook 'treemacs--select-icon-set)
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package company-box
    :hook (company-mode . company-box-mode))

  (evil-snipe-override-mode)
  (global-display-line-numbers-mode)

  ;; Workspace management binds unshamefully copied from Doom Emacs
  (evil-define-key nil evil-normal-state-map
    "gt" 'eyebrowse-next-window-config
    "gT" 'eyebrowse-prev-window-config)
  (spacemacs/set-leader-keys "TAB" (lambda () (interactive)
                                     (run-with-idle-timer
                                      0 nil '(lambda () (execute-kbd-macro (kbd "C-c C-w"))))
                                     (setq unread-command-events
                                           (listify-key-sequence (kbd "C-c C-w")))))

  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-vi-tilde-fringe-off)
  (spacemacs/toggle-golden-ratio))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(at-point 'bottom t)
 '(evil-escape-mode t)
 '(evil-want-Y-yank-to-eol nil)
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(flyspell-correct-ivy flyspell-correct auto-dictionary org-variable-pitch company-box all-the-icons-dired mixed-pitch sqlup-mode sql-indent csv-mode pandoc-mode ox-pandoc engine-mode highlight-indent-guides ivy-rich helm helm-core zenburn-theme zen-and-art-theme yaml-mode white-sand-theme web-mode web-beautify utop underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tuareg caml toxi-theme tide typescript-mode tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slime-company slime slim-mode seti-theme seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe reverse-theme rebecca-theme rbenv rake rainbow-mode rainbow-identifiers railscasts-theme purple-haze-theme pug-mode professional-theme prettier-js planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme ocp-indent occidental-theme obsidian-theme ob-elixir nodejs-repl noctilux-theme naquadah-theme mvn mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme minitest minimal-theme merlin-eldoc meghanada maven-test-mode material-theme majapahit-theme madhat2r-theme lush-theme lsp-java livid-mode skewer-mode light-soap-theme kaolin-themes json-navigator hierarchy json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme impatient-mode simple-httpd heroku-theme hemisu-theme helm-rtags helm-css-scss hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme groovy-mode groovy-imports pcache grandshell-theme gotham-theme google-c-style gandalf-theme flycheck-ycmd flycheck-rtags flycheck-ocaml merlin flycheck-credo flycheck-bashate flatui-theme flatland-theme fish-mode farmhouse-theme eziam-theme exotica-theme espresso-theme emojify emoji-cheat-sheet-plus emmet-mode dune dracula-theme doom-themes django-theme disaster darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme cpp-auto-include company-ycmd ycmd request-deferred company-web web-completion-data company-shell company-rtags rtags company-emoji company-c-headers common-lisp-snippets color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode clues-theme chruby chocolate-theme autothemer cherry-blossom-theme ccls busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist elixir-mode afternoon-theme org-journal treemacs-all-the-icons evil-collection annalist yapfify stickyfunc-enhance pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements lsp-python-ms live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-gtags helm-cscope xcscope ggtags cython-mode counsel-gtags counsel swiper ivy company-anaconda blacken anaconda-mode pythonic yasnippet-snippets xterm-color vterm unfill treemacs-magit terminal-here smeargle shell-pop orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink org-brain mwim multi-term magit-svn magit-section magit-gitflow magit-popup lsp-ui htmlize helm-org-rifle helm-org helm-lsp helm-gitignore helm-git-grep helm-company helm-c-yasnippet gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ fringe-helper git-gutter+ fuzzy flycheck-pos-tip pos-tip evil-snipe evil-org evil-magit magit git-commit with-editor transient eshell-z eshell-prompt-extras esh-help dap-mode posframe lsp-treemacs bui lsp-mode markdown-mode dash-functional company browse-at-remote auto-yasnippet yasnippet ac-ispell auto-complete ws-butler writeroom-mode winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil toc-org symon symbol-overlay string-inflection spaceline-all-the-icons restart-emacs request rainbow-delimiters popwin pcre2el password-generator paradox overseer org-superstar open-junk-file nameless move-text macrostep lorem-ipsum link-hint indent-guide hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio font-lock+ flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav editorconfig dumb-jump dotenv-mode diminish devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line))
 '(powerline-default-separator 'box)
 '(powerline-height 32)
 '(treemacs-width 25)
 '(wave 'arrow t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)
