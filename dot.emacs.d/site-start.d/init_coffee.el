;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_coffee.el

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

(provide 'init_coffee)

;;; END OF init_coffee.el
