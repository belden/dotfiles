(define-minor-mode belden/cperl-power-mode
  "Add a bunch of keybindings that I got used to at AirWave"
  :lighter " 登"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "s-a d w") 'delete-trailing-whitespace)
	    (define-key map (kbd "s-a m x") 'belden/cperl-power-mode/save-and-make-executable)
	    map))

(defun belden/cperl-power-mode/save-and-make-executable ()
  "Save the current buffer and make the file executable"
  (interactive)
  (if (buffer-file-name)
      (progn
	(save-buffer)
	(shell-command (concat "chmod 0755 " (buffer-file-name))))))

(provide 'belden/cperl-power-mode)
