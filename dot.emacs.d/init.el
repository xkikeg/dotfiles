;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;; use user site-lisp
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;; 受け取った引数をPATHに追加するもの
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path "lisp"
                  ;; 変更したり、自作の Emacs Lisp
                  "local-lisp"
                  ;; private 内には自分専用の物がはいっている
                  ;; 依存は private 内で完結するようにしている
                  "private"
                  ;; 初期設定ファイル
                  "site-start.d")

;; Emacs の種類バージョンを判別するための変数を定義
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))
(defvar emacs22-p (equal emacs-major-version 22))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar emacs24-p (equal emacs-major-version 24))
(defvar darwin-p (eq system-type 'darwin))
(defvar ns-p (featurep 'ns))
(defvar carbon-p (and (eq window-system 'mac) emacs22-p))
(defvar mac-p (and (eq window-system 'mac) (or emacs23-p emacs24-p)))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar colinux-p (when linux-p
                    (let ((file "/proc/modules"))
                      (and
                       (file-readable-p file)
                       (x->bool
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (re-search-forward "^cofuse\.+" nil t)))))))
(defvar cygwin-p (eq system-type 'cygwin))
(defvar nt-p (eq system-type 'windows-nt))
(defvar meadow-p (featurep 'meadow))
(defvar windows-p (or cygwin-p nt-p meadow-p))

;; 文字コード
;;(set-language-environment 'Japanese)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
;; 極力UTF-8とする
(cond
 (darwin-p
  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱う
  ;; 以下はファイル名を NFC で扱う環境と共同作業等する場合の対処
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (windows-p
  (setq file-name-coding-system 'sjis)
  (setq locale-coding-system 'utf-8))
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

;; ;enable font-lock
;; (when(fboundp 'global-font-lock-mode)(global-font-lock-mode t))
;; (setq font-lock-maximum-decoration t)

;; 全環境で有効な設定
(require 'init_global)

;; 環境依存設定
(cond
 (darwin-p (require 'init_darwin))
 )

;; 各種設定の起点
(require 'init_main)

;; custom-file
(setq custom-file
      (expand-file-name "private/customize.el" user-emacs-directory))

(req private)

;; 終了時バイトコンパイル
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (if (file-newer-than-file-p
                 (expand-file-name "init.el" user-emacs-directory)
                 (expand-file-name "init.elc" user-emacs-directory))
                (byte-compile-file
                 (expand-file-name "init.el" user-emacs-directory) 0))
            (byte-recompile-directory
             (expand-file-name "site-start.d" user-emacs-directory) 0)
            ))

;; END OF init.el
