(provide 'mediamath-go-mode)

(require 'go-mode)

(defun run-selected-golang-code ()
  "run the selected code as though it is golang code"
  (interactive)
  (let ((tempfile (format "%s.go" (make-temp-file "golang"))))
    (write-region (region-beginning) (region-end) tempfile)
    (compile (format "go run %s" tempfile))))

(define-derived-mode mediamath-go-mode go-mode
  "MM Go"
  (define-key mediamath-go-mode-map (kbd "C-x r r") 'run-selected-golang-code))
