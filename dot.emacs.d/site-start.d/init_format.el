(use-package apheleia
  :ensure t
  :config
  ;; Enable it globally (it handles most languages out of the box)
  (apheleia-global-mode +1))

(setf (alist-get 'rust apheleia-formatters)
      '("rustfmt" "--edition" (or (bound-and-true-p rust-edition) "2024")
        "--quiet" "--emit" "stdout"))

(provide 'init_format)
