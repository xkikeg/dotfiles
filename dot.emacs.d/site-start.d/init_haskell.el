;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_haskell.el

(custom-set-variables
 '(haskell-process-type 'cabal-dev)
 )

(autoload 'offside-trap-mode "offside-trap.el")

(add-hook
 'haskell-mode-hook
 '(lambda ()

    (turn-on-haskell-indentation)
    ; (turn-on-haskell-indent)

    (offside-trap-mode)
    ))


(provide 'init_haskell)

;;; END OF init_haskell.el
