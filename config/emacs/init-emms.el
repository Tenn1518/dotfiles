;;; init-emms.el --- Music player

(use-package emms
  :straight t
  :defer t
  
  :init
  (t/leader-def "M" '(:prefix-command t/leader-music-map :wk "music"))
  (general-def t/leader-music-map
    "m" #'emms
    "b p" #'emms-playlist-mode-go
    ;; music controls
    "p" #'emms-previous
    "P" #'emms-pause
    "n" #'emms-next
    ;; scrub through current song (repeatable)
    "<left>" #'emms-seek-backward
    "<right>" #'emms-seek-forward
    ;; browse by
    "b b" #'emms-smart-browse
    "b a" #'emms-browse-by-album
    "b A" #'emms-browse-by-artist
    ;; import
    "i d" #'emms-add-directory-tree
    "i f" #'emms-add-file)
  (general-def
    "s-<f7>" #'emms-previous
    "s-<f8>" #'emms-pause
    "s-<f9>" #'emms-next)

  :config
  ;; initialize emms
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (emms-all)
  (emms-default-players)
  (emms-cache-set-from-mpd-all)
  (setq emms-source-file-default-directory "~/Music/"
        emms-browser-covers 'emms-browser-cache-thumbnail) ; EMMS auto-resizes "cover.jpg" in album dir

  ;; repeat map for emms commands
  (defvar t/emms-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map [left] 'emms-seek-backward)
      (define-key map [right] 'emms-seek-forward)
      (define-key map "p" 'emms-previous)
      (define-key map "P" 'emms-pause)
      (define-key map "n" 'emms-next)
      map)
    "Keymap to repeat emms seeking through track sequences `C-c m <left>'.
Used in repeat mode.")
  (dolist (command '(emms-seek-backward emms-seek-forward))
    (put command 'repeat-map 't/emms-repeat-map)))

(provide 'init-emms)
;;; init-emms.el ends here
