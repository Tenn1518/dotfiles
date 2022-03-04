;;; init-theming.el --- Cycle through favorite themes

;; theme toggling with C-c h t
(defvar t/theme-list (list)
  "List of themes t/cycle-themes cycles through.")

(defvar t/theme--loaded nil
  "Currently loaded theme.")

(defun t/load-theme (THEME)
  "Wrapper for \"load-theme\".
Variable \"t/theme--loaded\" is set to THEME upon use."
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
    (disable-theme t/theme--loaded)
    (t/load-theme chosen-theme)
    (setq t/theme--loaded chosen-theme))
  (message "Loaded theme %s" t/theme--loaded))

(eval-after-load 'general
  (general-def t/leader-emacs-map "t" #'t/cycle-themes))

;; theme of choice
(use-package modus-themes
  :disabled
  :no-require t                      ; included in Emacs 28 but not as a library
  :hook (emacs-startup . (lambda () (t/load-theme 'modus-vivendi)))

  :init
  (setq modus-themes-italic-constructs t
        modus-themes-variable-pitch-headings t)
  (setq modus-themes-headings '((1 . (overline))
                                (2 . (overline))
                                (4 . (no-bold rainbow))
                                (t . (no-bold))))
  ;; set font settings for syntax
  (setq modus-themes-syntax '(yellow-comments alt-syntax))
  
  :config
  (dolist (theme '(modus-vivendi modus-operandi))
    (add-to-list 't/theme-list theme)))

;; Pack of themes provided by Doom Emacs
(use-package doom-themes
  :straight t

  :hook (after-init . (lambda () (t/load-theme 'doom-molokai)))

  :config
  (dolist (theme '(doom-molokai doom-solarized-light))
    (add-to-list 't/theme-list theme)))

;; Darken non-file buffers
(use-package solaire-mode
  :straight t

  :hook
  (after-init . solaire-global-mode))

(provide 'init-theming)
;;; init-theming.el ends here
