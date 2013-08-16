;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_c_and_cxx.el

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

;; C++
;; ヘッダファイル(.h)をc++モードで開く
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.inl$" . c++-mode))
              auto-mode-alist))

(provide 'init_c_and_cxx)

;;; END OF init_c_and_cxx.el
