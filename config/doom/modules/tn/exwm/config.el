;;; tn/exwm/config.el -*- lexical-binding: t; -*-

;; TODO - add all bindings to a SPC/SPC m mapping

(use-package! exwm
  :init
  (setq! exwm-workspace-number 10
         ;; exwm-input-global-keys
         ;; `(
         ;;   ;; 's-r': Reset (to line-mode).
         ;;   ([?\H-r] . exwm-reset)
         ;;   ;; 's-w': Switch workspace.
         ;;   ([?\H-w] . exwm-workspace-switch)
         ;;   ;; 's-&': Launch application.
         ;;   ([?\H-&] . (lambda (command)
         ;;                (interactive (list (read-shell-command "$ ")))
         ;;                (start-process-shell-command command nil command)))
         ;;   ;; 's-N': Switch to certain workspace.
         ;;   ,@(mapcar (lambda (i)
         ;;               `(,(kbd (format "H-%d" i)) .
         ;;                 (lambda ()
         ;;                   (interactive)
         ;;                   (exwm-workspace-switch-create ,i))))
         ;;             (number-sequence 0 9)))
         exwm-input-simulation-keys
         '(([?\C-b] . [left])
           ([?\C-f] . [right])
           ([?\C-p] . [up])
           ([?\C-n] . [down])
           ([?\C-a] . [home])
           ([?\C-e] . [end])
           ([?\M-v] . [prior])
           ([?\C-v] . [next])
           ([?\C-d] . [delete])
           ([?\C-k] . [S-end delete])))
  :hook (after-init . exwm-enable)
  :config
  ;; (add-hook 'exwm-update-class-hook
  ;;           (lambda ()
  ;;             (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Allow usage of SPC leader key in exwm buffers
  (add-to-list 'exwm-input-prefix-keys ? ))

;; firefox keybindings under C-c
(use-package! exwm-firefox
  :disabled
  :after exwm
  :config
  (exwm-firefox-mode))

;; edit text with the full power of emacs
(use-package! exwm-edit
  :after exwm
  :config
  (defun tn-exwm/on-exwm-edit-compose ()
    (funcall 'org-mode))
  (add-hook 'exwm-edit-compose-hook 'tn-exwm/on-exwm-edit-compose)
  (set-popup-rule! "^\\*exwm-edit" :size 0.4 :side 'bottom :select t))

;; control linux desktop with emacs keybindings
(use-package! desktop-environment
  :after exwm
  :config
  (desktop-environment-mode))

;; enable systemtray
(use-package! exwm-systemtray
  :after exwm
  :config
  (exwm-systemtray-enable))

;; helm
(when (featurep! :completion helm)
  (use-package! helm-exwm
    :after (helm exwm)
    :after-call (helm-exwm)))

(map! :leader
      ( :prefix-map ("e" . "exwm")
        (:when (featurep! :completion helm)
         :desc "Switch to buffer" "b" #'helm-exwm)
        :desc "Open application" "f" #'(lambda (command)
                                         (interactive (list (read-shell-command "$ ")))
                                         (start-process-shell-command command nil command))
        :desc "Reset to line-mode" "r" #'exwm-reset
        :desc "Restart exwm" "R" #'exwm-restart
        :desc "Quit exwm" "q" #'exwm-exit
        :desc "Switch workspace" "w" #'exwm-workspace-switch
        :desc "Compose in emacs" "c" #'exwm-edit--compose))
