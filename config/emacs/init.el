;;; init.el --- my emacs config
;; 5-3-2020

;;; Commentary:

;; A personal Emacs config.

;;; Code:

;; Settings

;; disable garbage collection until after init
(setq gc-cons-threshold-original gc-cons-threshold
	  gc-cons-threshold (* 1024 1024 100)
	  uniquify-buffer-name-style 'forward
      indent-tabs-mode nil
      require-final-newline t
      inhibit-startup-screen t
	  frame-inhibit-implied-resize t
	  backup-directory-alist `(("." . ,temporary-file-directory))
	  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory "~")

;; Font settings
(add-to-list 'default-frame-alist '(font . "Iosevka Medium-12"))
(set-face-attribute 'default nil :family "Iosevka Medium" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Medium" :height 120)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile Medium" :height 120)
;; modeline
(set-face-attribute 'mode-line nil :family "Iosevka Medium")

;; variable pitch in text-mode buffers
(add-hook 'text-mode-hook 'variable-pitch-mode)
;; frame size
(set-frame-size (selected-frame) 80 35)

(require 'uniquify)
(setq-default tab-width 4)

;; reenable disabled commands
(put 'narrow-to-region 'disabled nil)

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
(fringe-mode '(1 . 1))
;; highlight current line
(global-hl-line-mode)
;; set right command to control on macOS
(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control))

;; use ibuffer to list buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; GPG encryption
(when (string-equal system-type 'darwin)
    (setq epa-pinentry-mode 'loopback))

;; eshell settings
(defvar is-eterm-buffer nil
  "If set to t, the function 'my/eshell-exit' will exit the current frame.  Otherwise, it will run 'eshell/exit' and kill the current eshell buffer.")

(defun my/eterm ()
  "Opens separate frame specifically for eshell."
  (interactive)
  (make-frame-command)
  (set-frame-size (selected-frame) 90 24)
  (eshell 'new)
  (make-local-variable 'is-eterm-buffer)
  (setq mode-line-format nil)
  (setq-local is-eterm-buffer t)
  (select-frame-set-input-focus (selected-frame)))

(defun my/eshell-exit ()
  "Wrapper for eshell/exit that closes the frame if opened by my/eterm."
  (if is-eterm-buffer
      (delete-frame)
    (eshell/exit)))

(defun my/pwd ()
  "Return the last two or less directories of the current working directory."
  (let ((epwd (split-string (eshell/pwd)
							"/"))
		(home (getenv "HOME")))
    (if (<= (length epwd)
			2)
		(mapconcat 'identity epwd "/")
      (mapconcat 'identity (last epwd 2) "/"))))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (user-login-name) 'face '(:foreground "CadetBlue1"))
         (propertize "@" 'face '(:foreground "white"))
         (propertize (system-name) 'face '(:foreground "MediumPurple1"))
         " "
         (propertize (my/pwd) 'face '(:foreground "SpringGreen1"))
         " λ "))
      eshell-prompt-regexp "^.+?@.+? .+ \λ ")

;; Add to $PATH for eshell
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "~/.dotfiles/bin")
(add-to-list 'exec-path "~/.local/bin")
(setenv "PAGER" "cat")
(add-hook 'eshell-mode-hook '(lambda ()
							   (eshell/addpath "/usr/local/bin"
											   "/usr/local/sbin"
											   "~/.dotfiles/bin")
							   (set-window-margins nil 1)
							   (display-line-numbers-mode 0)))

;; Python mode
;; Set default Python shell to python3
(setq python-shell-interpreter "python3")
;; C++ mode
;; Set indenting settings
(setq c-default-style "bsd" c-basic-offset 4)
;; MHTML mode
;; Set default HTML indentation to 4 spaces
(add-hook 'html-mode-hook
		  (lambda ()
			(set (make-local-variable 'sgml-basic-offset) 4)))

;; youtube-dl
(defun tn/ytdl (url)
  "Call youtube-dl with argument URL and opens folder of files in wdired-mode.")

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

;; better, vim-like keybindings
(use-package general
  :straight t
  :demand
  :config
  (general-unbind '(normal visual motion)
				  "SPC")
  (general-create-definer tn/leader-def
	:states '(normal motion)
	:keymaps 'override
	:prefix "SPC"))

;; base16 themes for emacs
(use-package base16-theme
  :straight t
  :init
    (when (string-equal system-type 'darwin)
      (setq ns-use-srgb-colorspace nil))
  :config
  (load-theme 'base16-dracula t))

;; autocompletion
(use-package company
  :straight t
  :hook (after-init . global-company-mode))

;; better ui for autocompletion
(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;; check errors on the fly
(use-package flycheck
  :straight t
  :init
    (add-hook 'after-init-hook 'global-flycheck-mode))

;; Language Server Protocol support
(use-package lsp-mode
  :straight t
  :hook
  (python-mode . lsp)
  (mhtml-mode . lsp))

;; UI for LSP
(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-imenu-window-width 35)
  (lsp-ui-sideline-delay 2)
  (lsp-ui-sideline-show-diagnostics nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :hook (lsp-mode . lsp-ui-mode))

;; python
(use-package lsp-python-ms
  :straight t
  :after (lsp-mode))

;; autocompletion for emacs commands
(use-package ivy
  :straight t
  :demand
  :custom
  (ivy-re-builders-alist
   '((swiper-isearch . ivy--regex-plus)
	 (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode)
  :general
  (:states '(normal insert)
   :keymaps '(override ivy-minibuffer-map)
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line))

;; better ivy?
(use-package ivy-rich
  :straight t
  :after (ivy)
  :config
  (ivy-rich-mode 1))

;; ivy-like replacement for isearch
(use-package swiper
  :straight t
  :demand
  :after (ivy)
  :general
  (:states 'normal
  "/" 'swiper-isearch))

;; ivy replacements for emacs commands
(use-package counsel
  :straight t
  :demand
  :after (ivy)
  :config
  (counsel-mode)
  :general
  ('normal
	"C-p" 'counsel-yank-pop))

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
  :demand
  :after (dash goto-chg general)
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (general-evil-setup)
  :general
  (:state 'insert
		  "j" (general-key-dispatch 'self-insert-command
				:timeout 0.15
				"k" 'evil-normal-state))
  (tn/leader-def
	"h" 'evil-window-left
	"j" 'evil-window-down
	"k" 'evil-window-up
	"l" 'evil-window-right
	"1" 'delete-other-windows
	"2" 'split-window-below
	"3" 'split-window-right
	"0" 'delete-window))

(use-package evil-collection
  :straight t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-company-use-tng nil)
  :init (evil-collection-init))

;; vim motions without numbers
(use-package evil-easymotion
  :straight t
  :after (evil)
  :config
  (evilem-default-keybindings "'"))

;; inline searching with the s key
(use-package evil-snipe
  :straight t
  :after (evil)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

;; commenting with vim motions
(use-package evil-commentary
  :straight t
  :after (evil)
  :config
  (evil-commentary-mode))

;; mix variable- and fixed-width fonts when editing both text and code
(use-package mixed-pitch
  :straight t
  :hook
  (text-mode . mixed-pitch-mode))

;; org-mode
(use-package org
  :straight t
  :custom
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-M-RET-may-split-line nil)
  (org-indent-indentation-per-level 1)
  (org-adapt-indentation nil)
  (org-hide-emphasis-markers t)
  :hook (org-mode . (lambda ()
						   (buffer-face-mode)
						   (display-line-numbers-mode 0)
						   (set-window-margins nil 1)
						   (visual-line-mode))))

;; evil-integration for org-mode
(use-package org-evil
  :straight t
  :after (evil org evil-collection))

;; Fancy bullets in org-mode
(use-package org-bullets
  :straight t
  :after (org)
  :hook (org-mode . org-bullets-mode))

;; git manager
(use-package magit
  :straight t
  :general
  (tn/leader-def
	"g" 'magit))

;; evil integration for magit
(use-package evil-magit
  :straight t)

;; undo-tree
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

;; project manager
(use-package projectile
  :straight t
  :config
  (projectile-mode)
  :custom
  (projectile-project-search-path '("~/Code/"))
  :general
  (tn/leader-def
	:keymaps '(override projectile-mode-map)
	"p" 'projectile-command-map))

;; counsel integration for projectile
(use-package counsel-projectile
  :straight t)

;; smart managing of parentheses
(use-package paredit
  :straight t
  :after (org)
  :commands enable-paredit-mode
  ;;:config
  ;;(autoload 'enable-paredit-mode "paredit" nil t)
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup emacs-lisp-mode ielm-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

;; make sexps easier to distinguish
(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup emacs-lisp-mode ielm-mode lisp-mode lisp-interaction-mode scheme-mode) . rainbow-delimiters-mode))

;; fancy icons
(use-package all-the-icons
  :straight t)

;; fancy icons for dired
(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode 1)))
  (wdired-mode . (lambda () (all-the-icons-dired-mode 0)))
  :after (all-the-icons dired))

;; dependence for dired-hacks-*
(use-package dired-hacks-utils
  :straight t
  :after (dired))

;; dired colors
(use-package dired-rainbow
  :straight t
  :after (dired dired-hacks-utils)
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

;; indenting guide
(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; epub reader
(use-package nov
  :straight t)

;; features for editing common lisp
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))

;; my modeline
(use-package mode-line
  :straight nil
  :demand
  :load-path "lisp"
  :after (all-the-icons))

;; dashboard
(use-package dashboard
  :straight t
  :demand
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
		dashboard-center-content t
		dashboard-banner-logo-title "Tanzeem's Emacs"
		dashboard-startup-banner 'logo
		dashboard-set-heading-icons t
		dashboard-set-file-icons t
		dashboard-set-navigator t
		dashboard-footer-messages '("I showed you my source code, pls respond"
									"epic gamer moment")
		dashboard-footer-icon (all-the-icons-fileicon "emacs"
													  :height 1.0
													  :v-adjust -0.05
													  :face 'font-lock-keyword-face)

		dashboard-navigator-buttons
		`(;; line 1
		  ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
			"Github"
			"Open https://github.com/Tenn1518"
			(lambda (&rest _) (browse-url "https://github.com/Tenn1518"))
			))
		  ;; line 2
		  ((,(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust 0.0)
			"init.el"
			,(format "Open %s" user-init-file)
			(lambda (&rest _) (find-file user-init-file))
			))
		  )))

;; reenable garbage collection during idle
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (makunbound 'gc-cons-threshold-original)))

;;; init.el ends here
