; In your .emacs:
;
; ;; (load "offside-trap.el") is NOT good. It must be loaded 
; ;; after loading "haskell-indentation.el" or "haskell-indent.el"
; (autoload 'offside-trap-mode "offside-trap.el")))
;
; (add-hook 
;  'haskell-mode-hook
;  '(lambda ()
;
;     ;; Load one of you like.
;     ; (turn-on-haskell-indentation)
;     ; (turn-on-haskell-indent)
;
;     ;; (offside-trap-mode) must be called after turn-on-haskell-indentation/turn-on-haskell-indent,
;     ;; so that offside-trap-mode-map overrides haskell-indentation/indent-mode-map
;     (offside-trap-mode) 
;     ))
;
; ; For gnome-terminal / xfce4-terminal / too-gnome-friendly-terminal users:
; ;   The default key setting does not work nicely for gnomeish-terminals, since it does not 
; ;   detect C-return/C-tab. Just throw the terminal for newbie morons and 
; ;   use a good old terminal software such as xterm.

(eval-when-compile (require 'cl)) ; for `destructuring-bind'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic things

;; get the line string at the point
(defun offside-trap-string-of-line ()
  (buffer-substring-no-properties
   (point-at-bol) (point-at-eol)))

;; goto line
(defun offside-trap-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; line positions
(defun offside-trap-bol (line)
  (save-excursion
    (offside-trap-goto-line line)
    (point-at-bol)))

(defun offside-trap-eol (line)
  (save-excursion
    (offside-trap-goto-line line)
    (point-at-eol)))

;; move the point to the indent head of the line
(defun offside-trap-move-to-indent-head ()
  (goto-char (+ (point-at-bol) (current-indentation))))

;; move the point to the first non space char from the current pos
(defun offside-trap-move-to-non-space-char ()
  (let ((found (search-forward-regexp "[^ ]" (point-at-eol) t)))
    (if found (progn (backward-char) t) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; overlay

;; the overlay
(defvar offside-trap-overlay (make-overlay 1 1))
(defface offside-trap-face
  '((t (:background "#000055")))
  "Face for offside-trap highlight"
  :group 'offside-trap)
(overlay-put offside-trap-overlay 'face 'offside-trap-face)

;; put the overlay for the sticky lines
(defun offside-trap-overlay-sticky-lines (start end)
    (move-overlay offside-trap-overlay
		  start
		  end
		  (current-buffer)))

(defun offside-trap-empty-line  ()
  (string-match "^[ ]*$" (offside-trap-string-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sticky lines

;; if the lines match with the following regexp, they are skipped
;; in the sticky check.
;; CR: It is not perfect. ex: {- "{-" -}. But who cares? ;; -> now it's fixed
(defconst offside-trap-sticky-skip-lines-regexp
  (let ((empty "[ \t]*\n")
	(commented "[ \t]*--.*\n")
	(block-comment "[ \t]*{-\\([^-]\\|-[^}]\\)*-}[ \t]*\n"))
    (format "\\(%s\\|%s\\|%s\\)+" empty commented block-comment)))

(defun offside-trap-skip-sticky-skip-lines ()
  (if (save-excursion
	(move-beginning-of-line nil)
	(looking-at offside-trap-sticky-skip-lines-regexp))
      (progn
	(move-beginning-of-line nil)
	(re-search-forward offside-trap-sticky-skip-lines-regexp)
	;; (save-excursion (forward-char -1) (insert "XX"))
	t)
    nil))

;; check the line is "sticky"
(defun offside-trap-is-sticky-line (chars)
  (<= chars (current-indentation)))

;; find sticky lines and returns the end line number
;; point must be at the place where the future head of the indentation
(defun offside-trap-find-sticky-lines ()
  (setq result (line-number-at-pos))
  ;; if the line is empty, no search
  (if (offside-trap-empty-line) result

    ;; if not empty
    (let ((min-indent (current-column)))
      ;; (message (format "min-indent %d" min-indent))
      (save-excursion
	;; skip skippy lines at the head
	(if (not (offside-trap-skip-sticky-skip-lines))
	    ;; the first line is always sticky
	    (forward-line 1))
	(while
	    (if (offside-trap-skip-sticky-skip-lines)
		(not (= (point) (point-max)))
	      (if (offside-trap-is-sticky-line min-indent)
		  (progn
		    ;; (insert "<>")
		    (setq result (line-number-at-pos))
		    (if (= (point-at-eol) (point-max)) nil
		      (forward-line 1)
		      t))
		nil)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the trap

;; the state
(setq offside-trap-sticky-line-start nil)
(setq offside-trap-sticky-line-end nil)

;; find the sticky lines and set offside-trap-sticky-line-{start,end}
(defun offside-trap-set-sticky-lines ()
  (let* ((sticky-end (offside-trap-find-sticky-lines))
	 (sticky-start (line-number-at-pos)))
    (setq offside-trap-sticky-line-start sticky-start)
    (setq offside-trap-sticky-line-end sticky-end)
    (offside-trap-overlay-sticky-lines (point) (offside-trap-eol offside-trap-sticky-line-end))))
  
;; point must be at a point where f does not move it to the indent head 
;; the sticky lines must be registered in offside-trap-sticky-line-{start,end}
(defun offside-trap-block-indent-gen (f)
  (let ((old-lines (line-number-at-pos))
	(old-chars (current-column)))
    (apply f ()) ;; let the underlined function to indent the first line 
    ;; Line might be changed. Rebind offside-trap-block.
    (if offside-trap-sticky-line-start ;; if nil, do nothing
	(save-excursion
	  (let* ((new-lines (line-number-at-pos))
		 (new-chars (current-column))
		 (diff-lines (- new-lines old-lines))
		 (diff-chars (- new-chars old-chars)))
	    (progn 
	      (setq offside-trap-sticky-line-start (+ diff-lines offside-trap-sticky-line-start))
	      (setq offside-trap-sticky-line-end (+ diff-lines offside-trap-sticky-line-end))
	      ;; the first line is already indented 
	      (if (< offside-trap-sticky-line-start offside-trap-sticky-line-end)
		  (indent-rigidly (offside-trap-bol (1+ offside-trap-sticky-line-start))
				  (offside-trap-eol offside-trap-sticky-line-end)
				  diff-chars))
	      ))))))

;; step mode
;; offside-trap-sticky-line-{start,end} must be set
;; point must be already at the head of indentation 
(defun offside-trap-block-indent-step ()
  (interactive)
  (offside-trap-block-indent-gen 'indent-for-tab-command))

;; starting block indent 
(defun offside-trap-block-indent ()
  (interactive)
  ;; indent-for-tab-command moves the point to the indentation position, if it is left of it.
  (if (< (current-column) (current-indentation)) (offside-trap-move-to-indent-head))
  (offside-trap-set-sticky-lines)
  (setq overriding-terminal-local-map offside-trap-sticky-mode-map) ;; go into the sticky mode 
  (offside-trap-block-indent-step))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keymaps

;; push back the last event and exit from the offside trap mode
(defun offside-trap-other-char ()
  (interactive)
  (delete-overlay offside-trap-overlay)
  (setq overriding-terminal-local-map nil)
  ;; push back the keypress
  (setq unread-command-events
	(append (listify-key-sequence(this-command-keys))
		unread-command-events)))
 
;; offside trap sticky mode keymap (borrowed from isearch.el)
(defvar offside-trap-sticky-mode-map
  (let ((map (make-keymap)))
    (or (char-table-p (nth 1 map))
	(error "The initialization of offside-trap-sticky-mode-map must be updated"))
    ;; Default binding: exit
    (define-key map [t] 'offside-trap-other-char)
    (define-key map (kbd "<C-tab>") 'offside-trap-block-indent-step)
    (define-key map (kbd "TAB") 'offside-trap-block-indent-step)
    (define-key map (kbd "<tab>") 'offside-trap-block-indent-step)

    map)
  "Keymap for `offside-trap-sticky-mode'.")

;;;; haskell-indentation or haskell-indent

;; default is haskell-indent, since debian installs it as a default indentation mode
(defun is-haskell-indentation ()
  (and (boundp 'haskell-indentation-mode)
       haskell-indentation-mode))
 
(defun offside-trap-newline-and-block-indent ()
  (interactive)
  (if (is-haskell-indentation)
      (progn 
	;; haskell-indentation 
	(offside-trap-move-to-non-space-char)
	(offside-trap-set-sticky-lines)
	(setq overriding-terminal-local-map offside-trap-sticky-mode-map) ;; go into the sticky mode 
	(offside-trap-block-indent-gen 'haskell-newline-and-indent))
    ;; haskell-indent 
    ;; haskell-newline-and-indent first moves the point to the next non-empty char then insert a newline, 
    ;; which is bizarre... So first we perform this cursor movement by ourselves first.
    (offside-trap-move-to-non-space-char)
    (offside-trap-set-sticky-lines)
    (setq overriding-terminal-local-map offside-trap-sticky-mode-map) ;; go into the sticky mode 
    (offside-trap-block-indent-gen 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haskell-indentation

;; offside trap mode keymap (borrowed from isearch.el)
(defun offside-trap-mode-map ()
  (let ((keymap (make-sparse-keymap)))
    (if (is-haskell-indentation)
	(progn
	  ;; haskell-indentation
	  (define-key keymap (kbd "RET") 'offside-trap-newline-and-block-indent)
	  (define-key keymap (kbd "TAB") 'offside-trap-block-indent)
	  (define-key keymap (kbd "<tab>") 'offside-trap-block-indent)
	  ;; gnome-terminal users: stop using it. It does not catch contrl modifier with return or tab.
	  (define-key keymap (kbd "<C-return>") 'haskell-newline-and-indent)
	  (define-key keymap (kbd "<C-tab>") 'indent-for-tab-command))
      ;; haskell-indent
      (define-key keymap (kbd "RET") 'offside-trap-newline-and-block-indent)
      (define-key keymap (kbd "TAB") 'offside-trap-block-indent)
      (define-key keymap (kbd "<tab>") 'offside-trap-block-indent)
      ;; gnome-terminal users: stop using it. It does not catch control modifier with return or tab.
      (define-key keymap (kbd "<C-return>") 'newline)
      (define-key keymap (kbd "<C-tab>") 'indent-for-tab-command))
    keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; minor mode

(define-minor-mode offside-trap-mode
  "Offside trap mode for haskell."
  :lighter " OffsideTrap"
  :keymap (offside-trap-mode-map))
