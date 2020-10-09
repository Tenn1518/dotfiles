;; -*- no-byte-compile: t; -*-
;;; tn/exwm/packages.el

(package! xelb)
(package! exwm)
(package! exwm-firefox
  :disable t
  :recipe (:host github :repo "ieure/exwm-firefox"))
(package! exwm-edit
 :recipe (:host github :repo "agzam/exwm-edit"))
(package! desktop-environment)

(when (featurep! :completion helm)
  (package! helm-exwm
    :recipe (:host github :repo "emacs-helm/helm-exwm")))
