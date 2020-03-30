(provide 'belden/customization-mode)

(define-minor-mode belden/customization-mode
  "make it easier to customize things"
  :lighter " b/custom"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-f") 'customize-face)
	    map))
