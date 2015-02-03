(provide 'belden-scootch)
(require 'change-windows-intuitively)

(defun belden-scootch-right-or-find-win-right ()
  "DWIM movement: if you have an actively selected region, move that region of text to the right. Otherwise, move to the window that's to the right."
	(interactive)
	(if (region-active-p)
			(belden-scootch-right)
		(proff-select-window-right)))

(defun belden-scootch-left-or-find-win-left ()
  "DWIM movement: if you have an actively selected region, move that region of text to the left. Otherwise, move to the window that's to the left."
	(interactive)
	(if (region-active-p)
			(belden-scootch-left)
		(proff-select-window-left)))

(defun belden-scootch-up-or-find-win-up ()
  "DWIM movement: if you have an actively selected region, move that region of text to the up. Otherwise, move to the window that's to the up."
	(interactive)
	(if (region-active-p)
			(belden-scootch-up)
		(proff-select-window-up)))

(defun belden-scootch-down-or-find-win-down ()
  "DWIM movement: if you have an actively selected region, move that region of text to the down. Otherwise, move to the window that's to the down."
	(interactive)
	(if (region-active-p)
			(belden-scootch-down)
		(proff-select-window-down)))

(defun belden-scootch-up ()
  (interactive)
  (belden-scootch -1))

(defun belden-scootch-down ()
  (interactive)
  (belden-scootch 1))

(defun belden-scootch-left ()
  (interactive)
  (belden-scootch -1 't))

(defun belden-scootch-right ()
  (interactive)
  (belden-scootch 1 't))

(defun belden-scootch (linecount &optional horizontal)
  (let ((col (current-column)) mark-was-active beg end region)
    (if mark-active
        (progn
          (setq mark-was-active t)
          (if (< (point) (mark)) (exchange-point-and-mark))
          (setq beg (mark))
          (setq end (point))
          )
      (progn
        (setq beg (line-beginning-position))
        (setq end (1+ (line-end-position)))
        ))
    (setq region (buffer-substring beg end))
    (delete-region beg end)
    (if horizontal (forward-char linecount) (forward-line linecount))
    (setq beg (point))
    (insert region)
    (if mark-was-active 
        (progn
          (goto-char beg)
          (setq deactivate-mark 'nil)
          (set-mark (point))
          (goto-char (+ (point) (length region)))
          )
      (progn 
        (if horizontal (forward-char linecount) (forward-line linecount))
        (move-to-column col)
        ))))
