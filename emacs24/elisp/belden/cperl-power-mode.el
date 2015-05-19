(define-minor-mode belden/cperl-power-mode
  "Add a bunch of keybindings that I got used to at AirWave"
  :lighter " 登"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "s-a d w") 'delete-trailing-whitespace)
	    map))

(provide 'belden/cperl-power-mode)
