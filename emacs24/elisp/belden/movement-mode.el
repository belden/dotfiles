(define-minor-mode belden/movement-mode
  "Add a bunch of movement bindings that Belden likes."
  :lighter " 李"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-W M-J") 'windmove-down)
	    (define-key map (kbd "M-W M-H") 'windmove-left)
	    (define-key map (kbd "M-W M-K") 'windmove-up)
	    (define-key map (kbd "M-W M-L") 'windmove-right)
	    (define-key map (kbd "C-x \"") 'split-window-below)
	    (define-key map (kbd "C-x %") 'split-window-right)
	    (define-key map (kbd "C-x 1") 'delete-other-windows-vertically)
	    map))

(provide 'belden/movement-mode)
