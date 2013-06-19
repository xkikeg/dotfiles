;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_flymake.el

(require 'flymake)


(defun flymake-get-make-cmdline (source base-dir)
  "redefinition to remove 'check-syntax' target"
  (list "make"
        (list "-s" "-C"
              base-dir
              (concat "CHK_SOURCES=" source)
              "SYNTAX_CHECK_MODE=1"
              )))

(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  "force to check syntax of C/C++ without Makefile"
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init) ;; flymake built-in
    (flymake-simple-generic-init cmd opts)))

(defun flymake-simple-generic-init (cmd &optional opts)
  "Makefileがないときのコードチェック用関数"
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))

;; syntax checkが異常終了しても無視する
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted activate)
  (setq flymake-check-was-interrupted t))

;; C
(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only" "$CPPFLAGS")))

;; C++
(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-std=c++11" "-Wall" "-Wextra" "-pedantic" "-fsyntax-only" "$CPPFLAGS")))

(setq char_set "UTF-8")
(setq classpath
; (getenv "CLASS_PATH"))
nil)
(defun flymake-java-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   buffer-file-name
   'flymake-get-java-cmdline))
(defun flymake-get-java-cmdline
  (source base-dir)
  (list "javac"
        (if classpath
            (list "-classpath" classpath (concat "-J-Dfile.encoding=" char_set) "-encoding" char_set source)
          (list (concat "-J-Dfile.encoding=" char_set) "-encoding" char_set source))))

(push '("\\.[cCh]\\'" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.\\(?:cc\|cpp\|CC\|CPP\\)\\'" flymake-cc-init) flymake-allowed-file-name-masks)

(push '("\\.java$" flymake-java-init) flymake-allowed-file-name-masks)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (flymake-mode t)))

(add-hook 'java-mode-hook
          '(lambda ()
             (flymake-mode t)))

(provide 'init_flymake)

;;; END OF init_flymake.el
