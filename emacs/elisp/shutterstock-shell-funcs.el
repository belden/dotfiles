(provide 'belden-shell-funcs)

(defun belden-select-empty-output-buffer (buffername)
  (switch-to-buffer (get-buffer-create buffername))
  (belden-erase-buffer))
(defun belden-erase-buffer ()
  (setq buffer-read-only 'nil)
  (erase-buffer))

(defun belden-shell-function (cmd &optional buffername quiet)
  "Run a function defined in our bash configuration"
  (interactive (list (read-string "% ")))
  (if (not buffername) (setq buffername "*shell function output*"))
  (if (string= buffername "discard output") (setq buffername 'nil))
  (if (string= buffername "stdout")
      (setq buffername 't)
    (if buffername (belden-select-empty-output-buffer buffername)))
  (when (not quiet) (message "Running: %s" cmd))
  (call-process "bash" nil buffername nil
                "--noprofile" "-O" "expand_aliases" "-l" "-c"
                (format "cd %s; %s" (root) cmd))
  (when (not quiet) (message "Done.")))

(defun belden-shell-function-eval (cmd)
  "Evaluate a function and return its output"
  (with-output-to-string
    (with-current-buffer
        standard-output
      (belden-shell-function cmd "stdout" 't))))

(defun belden-shell-function-insert (command)
	(insert (belden-shell-function-eval command)))

(defun belden-shell-replace (shell-replace-command)
  "Replace region with output of shell command on region"
  (interactive
   (list (read-string "Replace region with: ")))
  (let ((beg (if mark-active (mark) (point))))
    (shell-command-on-region beg (point) shell-replace-command nil t)))
