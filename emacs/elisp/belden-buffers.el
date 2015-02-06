(provide 'belden-buffers)

(defun belden-kill-this-buffer ()
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))
        (if (> (count-windows) 1) (delete-window))
        ))))

(defun belden-hide-this-buffer ()
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
				(delete-window)))))

(defun belden-kill-other-buffer ()
  (interactive)
  (other-window 1)
  (belden-kill-this-buffer))

(defun belden-save-other-buffer (buffer)
  (with-current-buffer buffer
    (progn
      (set-buffer-modified-p 't)
      (save-buffer))))
