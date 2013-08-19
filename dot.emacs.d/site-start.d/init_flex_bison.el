;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_flex_bison.el

(autoload 'bison-mode "bison-mode" "Major mode for editing Yacc/Bison code" t)
;; *.y *.yy ファイルを 自動的に bison-mode にする
(add-to-list 'auto-mode-alist
             '("\\.\\(y\\|yy\\)$" . bison-mode))

(autoload 'flex-mode "flex-mode" "Major mode for editing Lex/Flex code" t)
;; *.l *.ll ファイルを 自動的に flex-mode にする
(add-to-list 'auto-mode-alist
             '("\\.\\(l\\|ll\\)$" . flex-mode))

(provide 'init_flex_bison)

;;; END OF init_flex_bison.el
