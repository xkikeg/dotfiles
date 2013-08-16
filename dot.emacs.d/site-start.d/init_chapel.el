;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_chapel.el

(setq chpl-home (getenv "CHPL_HOME"))
(autoload 'chpl-mode "chpl-mode" "Chpl enhanced cc-mode" t)
(add-to-list 'auto-mode-alist '("\\.chpl$" . chpl-mode))
(add-hook 'chpl-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (setq tab-width c-basic-offset)
             (setq indent-tabs-mode t)))

(provide 'init_chapel)

;;; END OF init_chapel.el
