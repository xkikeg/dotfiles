;use user site-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; when and require
(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))

;enable font-lock
(when(fboundp 'global-font-lock-mode)(global-font-lock-mode t))
(setq font-lock-maximum-decoration t)

;use UTF-8
;(if (= emacs-major-version 21)
;    (require 'un-define))
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info "Japanese" 'coding-priority(cons 'utf-8(get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;use UTF-8
(add-hook 'shell-mode-hook(lambda()(set-buffer-process-coding-system 'utf-8 'utf-8)))

; 括弧の対応
(show-paren-mode t)

; highlight selected region
(transient-mark-mode t)

; set tab number
(setq-default tab-width 4)

; Use space instead of tab
(setq-default indent-tabs-mode nil)

;use bash
(setq explicit-shell-file-name "/bin/bash") 
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")

;;; Set C-h As Backspace
(global-set-key "\C-h" 'delete-backward-char)

;hide inputting password
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;handle escape sequences
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; unset prefix preserve: default is now C-c C-t hoge
;(setq YaTeX-inhibit-prefix-letter nil)
;use ams-LaTeX on YaTeX
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(add-hook 'yatex-mode-hook '(lambda () (reftex-mode t)))
; UTF-8
(setq YaTeX-kanji-code nil)
; dviからpdfを作成する%sはファイル名
(setq dviprint-command-format "dvipdfmx %s")
;font change
;(add-to-list 'YaTeX-hilit-patterns-alist
;	     '(YaTeX-19-region-section-type "\\\\textgt\\>" bold))
;(add-to-list 'YaTeX-hilit-patterns-alist
;	     '(YaTeX-19-region-section-type "\\\\textit\\>" italic))
;(add-to-list 'YaTeX-hilit-patterns-alist
;	     '(YaTeX-19-region-section-type "\\\\textsl\\>" italic))
;(add-to-list 'YaTeX-hilit-patterns-alist
;	     '(YaTeX-19-region-section-type "\\\\emph\\>" italic))
;(add-to-list 'YaTeX-hilit-patterns-alist
;	     '(YaTeX-19-region-section-type "\\\\texttt\\>" tt))
; Set my environment
(setq YaTeX-package-alist-private'(
 ("ikgmath"
  (env . YaTeX-package-ams-envs)
  (section "tag" "tag*"))
 ("ikgutil"
  (section "wordlangset"))
 ("ikgcommon"
  (section "includegraphics"
       "rotatebox" "scalebox" "resizebox" "reflectbox"))
))
;; use YaTeX for hoge.tex file.
(req yatex)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))

;; C, C++のインデント設定
(add-hook 'c-mode-common-hook
           '(lambda ()
             (c-set-style "stroustrup")
             (setq c-basic-offset 2)
	     (c-set-offset 'inline-open 0)))
(add-hook 'c++-mode-common-hook
           '(lambda ()
             (c-set-style "stroustrup")
             (setq c-basic-offset 2)
             (c-set-offset 'inline-open 0)))

; C++
; ヘッダファイル(.h)をc++モードで開く
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.inl$" . c++-mode))
              auto-mode-alist))

; Cilk
; cilk files also included in c++-mode
(setq auto-mode-alist
      (append '(("\\.cilkh$" . c++-mode))
              auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.cilk$" . c++-mode))
              auto-mode-alist))

; using chpl-mode for Chapel
(setq chpl-home (getenv "CHPL_HOME"))
; make sure that when chpl-mode is entered, (our modified) cc-mode is
; loaded
(autoload 'chpl-mode "chpl-mode" "Chpl enhanced cc-mode" t)
; make loading files with a .chpl extension put emacs into chpl-mode
(add-to-list 'auto-mode-alist '("\\.chpl$" . chpl-mode))
(add-hook 'chpl-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (setq tab-width c-basic-offset)
             (setq indent-tabs-mode t)))

;; package specific config

;; valgrind code
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p ".*/valgrind/.*" (concat buffer-file-name) 0)
              (make-local-variable 'c-basic-offset)
              (setq c-basic-offset 3))
            ))

; imaxima
(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)

; using graph-viz-mode
(load "graphviz-dot-mode.el")
(add-hook 'graphviz-dot-mode-hook (lambda () (local-set-key [f5] "\C-x\C-s\C-cc\C-m\C-cp")))

; Use C/Migemo
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(setq migemo-regex-dictionary nil)
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(if (condition-case nil
	(require 'migemo)
      (file-error))
    (migemo-init))

;; Do not create backup file in Dropbox dir.
(let ((dropbox-directory (expand-file-name "~/Dropbox/"))
      (destination-directory temporary-file-directory))
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat dropbox-directory "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat destination-directory "\\2") t))
  (add-to-list 'backup-directory-alist
               `(,dropbox-directory . ,destination-directory)))

;; OCaml
(setq auto-mode-alist (cons '("\.ml\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'tuareg-run-caml "tuareg" "startup a Caml toplevel" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; Evernote
(req epa-setup)
(req evernote-mode)
(setq evernote-username "liquid_amber")
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)
