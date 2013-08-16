;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_migemo.el

;; Use C/Migemo
(setq migemo-options '("-q" "--emacs"))
(setq migemo-regex-dictionary nil)
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(cond
 (darwin-p
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-dictionary
        "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict"))
 (t
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
(if (condition-case nil
	(require 'migemo)
      (file-error))
    (migemo-init))

(provide 'init_migemo)

;;; END OF init_migemo.el
