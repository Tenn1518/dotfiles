;;; init-evil.el --- Evil mode                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  

;; Author: tnazmee

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

(use-package evil
  :straight t

  :hook
  (after-init . evil-mode)
  (org-capture-mode . evil-insert-state)

  :custom
  (evil-undo-system #'undo-redo)      ; enable redo in Emacs 28+
  (evil-want-keybinding nil)          ; evil-collection requirement
  (evil-want-Y-yank-to-eol t)         ; Y and yy to copy line is redundant
  (evil-mode-line-format '(after . mode-line-remote))
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-symbol-word-search t)

  :config
  ;; `C-g' removes search highlights
  (advice-add #'keyboard-quit :before #'evil-ex-nohighlight)
  ;; use evil's search over isearch
  (evil-select-search-module 'evil-search-module 'evil-search))

;; manip delims
(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Vertical align
(use-package evil-lion
  :straight t
  :after evil
  :config
  (general-def
    :keymaps 'global
    :states '(normal visual)
    "g l" #'evil-lion-left
    "g L" #'evil-lion-right))

;; comment out text objects with `gc'
(use-package evil-nerd-commenter
  :straight t
  :after evil
  :config
  (general-def
    :keymaps 'global
    :states 'normal
    "gc" #'evilnc-comment-operator))

;; operate on argument list items
(use-package evil-args
  :straight t
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(provide 'init-evil)
;;; init-evil.el ends here
