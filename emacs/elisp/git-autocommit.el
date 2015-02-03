(provide 'git-autocommit)

;; Automatically add, commit, and push when files change.

(defun autocommit-after-save-hook ()
  "After-save-hook to 'git add' the modified file and schedule a commit and push in the idle loop."
  (let ((fn (string-replace-match (concat (getenv "code_root") "/") (buffer-file-name) ""))
						(dn (file-name-directory (buffer-file-name))))
    (message "git adding %s" fn)
		(shell-command (format "cd %s; git add %s; git commit -m 'Updated %s'" (getenv "code_root") fn fn))))

(defun autocommit-setup-save-hook ()
  "Set up the autocommit save hook for the current file."
  (interactive)
  (message "Set up autocommit save hook for this buffer.")
  (add-hook 'after-save-hook 'autocommit-after-save-hook nil t))