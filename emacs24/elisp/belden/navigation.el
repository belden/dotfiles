(provide 'belden/navigation)

(defun belden/navigation/forward-word ()
   "Move one word forward. Leave the pointer at start of word
   instead of emacs default end of word. Treat _ as part of word"
   (interactive)
   (let ((start (point)) boundary at-boundary jump)
     (setq at-boundary (eolp))
     (setq boundary (line-end-position))
     (forward-char 1)
     (backward-word 1)
     (forward-word 2)
     (backward-word 1)
     (backward-char 1)
     (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) (forward-char 1) 
            (belden-forward-word))
           (t (forward-char 1)))
     (if (and (not at-boundary) (> (point) boundary)) (goto-char boundary))
     (setq jump (belden-count-lines start (point)))
     (if (> jump 0) (progn (forward-line (- 1 jump)) (back-to-indentation)))
     ))

(defun belden/navigation/backward-word ()
   "Move one word backward. Leave the pointer at start of word
   Treat _ as part of word."
   (interactive)
   (let ((start (point)) boundary at-boundary jump)
     (setq at-boundary (empty-line-prefix))
     (setq boundary (line-beginning-position))
     (backward-word 1)
     (backward-char 1)
     (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) 
            (belden-backward-word))
           (t (forward-char 1)))
     (if (and (not at-boundary) (< (point) boundary)) 
         (progn (goto-char boundary) (back-to-indentation)))
     (setq jump (belden-count-lines start (point)))
     (if (> jump 0) (progn (forward-line (- jump 1)) (end-of-line)))
     ))

(defun belden/next-error-recenter ()
   (interactive)
   (let ((code-window (selected-window)))
     (next-error)
     (pop-to-buffer (compilation-find-buffer))
     (recenter)
     (select-window code-window)
     (recenter)
     )
   )

(defun belden/previous-error-recenter ()
   (interactive)
   (let ((code-window (selected-window)))
     (previous-error)
     (pop-to-buffer (compilation-find-buffer))
     (recenter)
     (select-window code-window)
     (recenter)
     )
   )
