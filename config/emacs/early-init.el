;;; early-init.el --- Initial startup optimizations  -*- lexical-binding: t; -*-

;;; Commentary:

;; This code is ran extremely early during Emacs's startup.  Code prevents Emacs
;; from loading unnecessary packages or undertaking complex, time-wasting
;; actions.

;;; Code:

;; disable garbage collection until post-init
(setq gc-cons-threshold-original gc-cons-threshold
      gc-cons-threshold (* 1024 1024 100))

;; Stop Emacs's default package system from initializing
(setq package-enable-at-startup nil)

;; Stop text-mode from eager loading some packages
(setq initial-major-mode 'fundamental-mode)

;; Stop Emacs from mangling the frame on startup
(setq frame-inhibit-implied-resize t)

;; Stop Emacs from checking each file for major mode during startup
(defvar t/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(provide 'early-init)
;;; early-init.el ends here
