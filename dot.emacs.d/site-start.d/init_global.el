;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_global.el

;; 括弧の対応
(show-paren-mode t)

;; リージョン設定時にハイライトされる設定
(transient-mark-mode t)

;; タブを4文字に設定
(setq-default tab-width 4)

;; タブの代わりにスペースを使用
(setq-default indent-tabs-mode nil)

;; use bash
(setq explicit-shell-file-name "/bin/bash") 
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")

;; C-hでバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; 入力中のパスワードを隠す
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; 色のエスケープシーケンスの扱い
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; 存在する場合のみrequireする
(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))

(provide 'init_global)

;;; END OF init_global.el
