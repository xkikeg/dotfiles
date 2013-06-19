;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; BEGIN OF init_yatex.el

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

(provide 'init_yatex)

;;; END OF init_yatex.el
