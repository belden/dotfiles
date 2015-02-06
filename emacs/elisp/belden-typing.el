(provide 'belden-typing)
(require 'belden-func)

(defun belden-follow-mouse (event)
  "Jump somewhere else based on where the mouse event started"
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (car (cdr (event-start event))))
  (belden-follow))
 
(defun belden-keyboard-quit ()
  (interactive)
  (when (not mark-active) (widen))
  (keyboard-quit))

(defun belden-random-string ()
  (interactive)
  (insert (direct-command-to-string (concat (getenv "HOME") "/bin/development-tools/random-string") "--no-newline")))

(random t) ; seed
(defun belden-random-number (max)
  (interactive "p")
  (if (= max 1) (setq max 10000))
  (insert (format "%s" (random max))))

(defun belden-add-a-block ()
  "Insert a code block, i.e. { } move point to the first line inside the block"
	(interactive)
	(insert " {\n\t}")
	(previous-line)
	(end-of-line)
	(insert "\n\t\t"))
