;;; Tree-sitter --- Code parsing of the future

;; tree sitter
(use-package tree-sitter
  :straight t
  :hook
  ((prog-mode . turn-on-tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package tree-sitter-indent
  :straight t
  :after tree-sitter)

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
