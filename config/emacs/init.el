;;; init.el --- my emacs config
;; 5-3-2020

;;; Commentary:

;; A personal Emacs config.

;;; Code:

;; Settings
;;=========

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      indent-tabs-mode nil
      require-final-newline t
      inhibit-startup-screen t
      default-directory "~")
;;      setq find-file-visit-truename t)
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
(fringe-mode '(0 . 0))
;; highlight current line
(global-hl-line-mode)

;; Font settings
(add-to-list 'default-frame-alist '(font . "Iosevka Medium-12"))

;; Keep backups and autosaves in /tmp
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
	(if )
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
  (general-create-definer tn/leader-def
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
  :init
    (add-hook 'after-init-hook 'global-company-mode))

;; check errors on the fly
(use-package flycheck
  :straight t
  :init
    (add-hook 'after-init-hook 'global-flycheck-mode))

;; Language Server Protocol support
(use-package lsp-mode
  :straight t
  :hook python-mode)

;; UI for LSP
(use-package lsp-ui
  :straight t
  :hook python-mode)

;; python
(use-package lsp-python-ms
  :straight t
  :after (lsp-mode)
  :hook python-mode)

;; autocompletion for emacs commands
(use-package ivy
  :straight t
  :demand
  :config
  (setq ivy-re-builders-alist
		'((swiper-isearch . ivy--regex-plus)
		  (t . ivy--regex-fuzzy)))
  (ivy-mode)
  :general
  (:keymaps '(ivy-minibuffer-map override)
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line))

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
  (tn/leader-def
	:keymaps 'normal
	"p" 'counsel-yank-pop))

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
  :config
  (evil-mode 1)
  (general-evil-setup)
  :general
  (:state 'insert
		  "j" (general-key-dispatch 'self-insert-command
				:timeout 0.10
				"k" 'evil-normal-state))
  (general-unbind 'motion
	"SPC")
  (tn/leader-def
	'(normal motion visual)
	:keymaps 'override
	"h" 'evil-window-left
	"j" 'evil-window-down
	"k" 'evil-window-up
	"l" 'evil-window-right
	"1" 'delete-other-windows
	"2" 'split-window-below
	"3" 'split-window-right
	"0" 'delete-window))
  
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

;; org-mode
(use-package org
  :straight t
  :config
  (setq org-startup-indented t
		org-image-actual-width nil
		org-M-RET-may-split-line nil)
  :hook (org-mode-hook . (lambda ()
						   (buffer-face-mode)
						   (setq buffer-face-mode-face '(:family "Iosevka Aile Medium"))
						   (display-line-numbers-mode 0)
						   (visual-line-mode))))

;; evil-integration for org-mode
(use-package org-evil
  :straight t
  :after (evil org))

;; better ivy?
(use-package ivy-rich
  :straight t
  :after (ivy)
  :config
  (ivy-rich-mode 1))

;; git manager
(use-package magit
  :straight t)

;; undo-tree
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

;; Fancy bullets in org-mode
(use-package org-bullets
  :straight t
  :after (org)
  :hook org-mode)

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
  :hook (dired-mode . all-the-icons-dired-mode)
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

;; my modeline
(use-package mode-line
  :straight nil
  :demand
  :load-path "lisp"
  :after (all-the-icons))

;; Keybindings
;;============

;; set right command to control on macOS
(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control))

;; open an eshell instance in a new frame, mimicking a standard terminal
(global-set-key (kbd "C-c t") 'my/eterm)
;; use ibuffer to list buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; init.el ends here
