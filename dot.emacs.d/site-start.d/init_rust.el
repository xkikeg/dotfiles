(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #rust

(use-package yasnippet
  :ensure t)

(use-package company
  :ensure t)

(use-package rust-mode
  :ensure t)


(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
  ; intended double --.
  :init (setq cargo-process--command-check "check --workspace --all-targets --all-features"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #lsp

;; (use-package lsp-mode
;;   :ensure t
;;   :init (yas-global-mode)
;;   :init (setq lsp-keymap-prefix "M-l")
;;   :hook (rust-mode . lsp)
;;   :init (require 'bind-key)
;;   :bind ("C-c h" . lsp-describe-thing-at-point)
;;   :custom (lsp-rust-analyzer-lru-capacity 128)
;;   :custom (lsp-rust-server 'rust-analyzer)
;;   :custom (lsp-rust-analyzer-server-command (split-string "systemd-run --scope -p MemoryMax=1G --user rust-analyzer"))
;;   )

;; (use-package lsp-ui
;;   :ensure t)

(provide 'init_rust)
