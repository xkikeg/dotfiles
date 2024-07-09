;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_global.el

;; 括弧の対応
(show-paren-mode t)

;; リージョン設定時にハイライトされる設定
(transient-mark-mode t)

;; ファイル末尾の改行を強制
(setq require-final-newline t)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

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

;; Don't ask when following vcs symlink
(setq vc-follow-symlinks t)

;; use-packageだけを使えるようにして
;; ほかはすべてuse-packageに依存
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ;; el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (el-get 'sync)

;; Do not create backup file in Dropbox dir.
(let ((dropbox-directory (expand-file-name "~/Dropbox/"))
      (destination-directory temporary-file-directory))
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat dropbox-directory "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat destination-directory "\\2") t))
  (add-to-list 'backup-directory-alist
               `(,dropbox-directory . ,destination-directory)))

(provide 'init_global)

;;; END OF init_global.el
