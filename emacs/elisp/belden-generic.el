(provide 'belden-generic)

(defun belden-update-buffers ()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list)) (buffer (car list)) errmesg)
    (loop for buffer in (buffer-list) do
          (if (and (not (string-match "\\*" (buffer-name buffer)))
                   (buffer-file-name buffer)
                   (file-exists-p (buffer-file-name buffer)))
              (if (and (not (verify-visited-file-modtime buffer)) ; been touched
                       (buffer-modified-p buffer)) ; and modified 
                  (setq errmesg (concat errmesg 
                    (format "Buffer '%s' has file and buffer changes!\n" buffer)))
                (belden-update-buffer buffer))))
    (message "%s" (or errmesg "Done refreshing all open non-modified files..."))))

(defun belden-update-buffer (buffer)
  (set-buffer buffer) 
  (message "Refreshing %s" (buffer-file-name buffer))
  (if (not (verify-visited-file-modtime buffer))
      (if (buffer-modified-p buffer) (error "Buffer has file and buffer changes")
        (revert-buffer t t t))) ; revert if touched and not modified
  (vc-file-clearprops (buffer-file-name)))
