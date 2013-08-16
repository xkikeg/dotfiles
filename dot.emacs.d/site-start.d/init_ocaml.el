;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_ocaml.el

;; OCaml
(setq auto-mode-alist (cons '("\.ml\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'tuareg-run-caml "tuareg" "startup a Caml toplevel" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(provide 'init_ocaml)

;;; END OF init_ocaml.el
