(provide 'belden/cperl-power-mode)
(define-minor-mode belden/cperl-power-mode
  "Add a bunch of keybindings that I got used to at AirWave"
  :lighter " 登"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "s-a d w") 'delete-trailing-whitespace)
	    (define-key map (kbd "s-a h ~") 'hide-lines-matching)
	    (define-key map (kbd "s-a h !") 'hide-lines-not-matching)
	    (define-key map (kbd "s-a h 0") 'hide-lines-show-all)

	    ;; toggle on/off my various minor modes
	    (define-key map (kbd "s-a m h") 'belden/hotkeys-mode)
	    (define-key map (kbd "s-a m m") 'belden/movement-mode)
	    (define-key map (kbd "s-a m p") 'belden/cperl-power-mode)

	    (define-key map (kbd "s-a m x") 'belden/cperl-power-mode/save-and-make-executable)
	    (define-key map (kbd "s-a p d") 'cperl-perldoc)
	    (define-key map (kbd "s-a p m") 'belden-cperl-mode)
	    (define-key map (kbd "s-a r n") '(lambda () (interactive) (random)))
	    (define-key map (kbd "s-a r i") '(lambda () (interactive) (random 2147483647)))
	    (define-key map (kbd "s-a r s") 'belden/random-string)
	    (define-key map (kbd "s-a s i") 'belden/shell-insert)
	    (define-key map (kbd "s-a u b") 'belden/cperl-power-mode/update-buffers)
	    map))

(require 'hide-lines)
(defalias 'show-all-invisible 'hide-lines-show-all)
(defalias 'hide-matching-lines 'hide-lines-matching)
(defalias 'hide-non-matching-lines 'hide-lines-not-matching)

(defun belden/cperl-power-mode/save-and-make-executable ()
  "Save the current buffer and make the file executable"
  (interactive)
  (if (buffer-file-name)
      (progn
	(save-buffer)
	(shell-command (concat "chmod 0755 " (buffer-file-name))))))

(defun belden/cperl-power-mode/update-buffers ()
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
                (belden/cperl-power-mode/update-buffer buffer))))
    (message "%s" (or errmesg "Done refreshing all open non-modified files..."))))

(defun belden/cperl-power-mode/update-buffer (buffer)
  (set-buffer buffer)
  (message "Refreshing %s" (buffer-file-name buffer))
  (if (not (verify-visited-file-modtime buffer))
      (if (buffer-modified-p buffer) (error "Buffer has file and buffer changes")
        (revert-buffer t t t))) ; revert if touched and not modified
  (vc-file-clearprops (buffer-file-name)))

(defun belden/shell-insert (belden-shell-command)
  "Run a shell command and insert its contents, removing trailing newline"
  (interactive
   (list (read-string "shell command: ")))
  (insert (replace-regexp-in-string "\n$" ""
				    (shell-command-to-string belden-shell-command))))

(defun belden/random-string ()
  (interactive)
  (belden/shell-insert "random-string"))
