(provide 'rn-belden-js-mode)

(require 'js2-mode)

(defun rn-belden-js-run-file-locally ()
  "run the current file as though it is js code"
  (interactive)
  (save-some-buffers)
  (compile (format "node %s" (buffer-file-name))))

(define-derived-mode rn-belden-js-mode js2-mode
  "runnableJS"
  (define-key rn-belden-js-mode-map (kbd "C-x r f") 'rn-belden-js-run-file-locally)
  )
