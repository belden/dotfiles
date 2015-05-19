(provide 'belden-cperl-mode)

(define-derived-mode belden-cperl-mode cperl-mode
  "Belden CPerl"
  (define-key belden-cperl-mode-map "\t" 'belden-indent-cperl-region-or-line)
)

(defun current-line-prefix ()
  (buffer-substring (line-beginning-position) (point)))
(defun current-line-suffix () (buffer-substring (point) (line-end-position)))
(defun empty-line-suffix () (only-whitespace (current-line-suffix)))
(defun empty-line-prefix () (only-whitespace (current-line-prefix)))
(defun only-whitespace (str) (and (string-match "^[ \r\t]*\$" str) 't))

(defun previous-char ()
  (char-to-string (preceding-char)))

(defun previous-key (&optional arg)
  (if (not arg) (setq arg 1))
  (let (recent-keys index)
    (setq recent-keys (recent-keys))
    (setq index (- (length recent-keys) (1+ arg)))
    (if (>= index 0)
	(aref recent-keys index)
      'nil)))

(defun belden-indent-cperl-region-or-line ()
  (interactive)
  (belden-generic-indent-region-or-line (function cperl-indent-command)))

(defun belden-generic-indent-region-or-line (indent-command)
  (if mark-active
      (progn  ; set beg end to marked region
	(if (< (point) (mark)) (exchange-point-and-mark))
	(cperl-indent-region (mark) (1- (point)))
	)
    (let ((previous-key (previous-key)))
      (if (and (or (looking-at "\\>")
		   (empty-line-suffix)
		   (string-equal (previous-char) ":")
		   (eq previous-key 'left)
		   (eq previous-key 'right))
	       (not (eq previous-key 'down))
	       (not (eq previous-key 'up))
	       (not (and (eolp) (string-match "[\]\)\}\,]" (previous-char))))
	       (not (empty-line-prefix)))
	  (hippie-expand 'nil)
	(progn
	  (setq he-num -1)
	  (funcall indent-command))))))

