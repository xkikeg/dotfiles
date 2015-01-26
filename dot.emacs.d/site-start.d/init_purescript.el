;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_purescript.el

(add-hook 'purescript-mode-hook
          '(lambda ()
             (turn-on-purescript-indentation)
             ))

(provide 'init_purescript)

;;; END OF init_purescript.el
