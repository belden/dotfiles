(provide 'belden/cperl-power-mode)
(define-minor-mode belden/cperl-power-mode
  "Add a bunch of keybindings that I got used to at AirWave"
  :lighter " 登"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "s-a = >") 'belden/align-to-fat-arrow)
	    (define-key map (kbd "s-a c s") 'belden/comparesub)
	    (define-key map (kbd "s-a d d") 'belden/set-buffer-default-directory)
	    (define-key map (kbd "s-a d w") 'delete-trailing-whitespace)
	    (define-key map (kbd "s-a h ~") 'hide-lines-matching)
	    (define-key map (kbd "s-a h !") 'hide-lines-not-matching)
	    (define-key map (kbd "s-a h 0") 'hide-lines-show-all)

	    ;; understand more about the code you're looking at, using B:: modules
	    (define-key map (kbd "s-a B c") 'cperl-concise-region)
	    (define-key map (kbd "s-a B d") 'cperl-deparse-region)

	    ;; toggle on/off my various minor modes
	    (define-key map (kbd "s-a m h") 'belden/hotkeys-mode)
	    (define-key map (kbd "s-a m m") 'belden/movement-mode)
	    (define-key map (kbd "s-a m p") 'belden/cperl-power-mode)

	    (define-key map (kbd "s-a m x") 'belden/cperl-power-mode/save-and-make-executable)
	    (define-key map (kbd "s-a o p") 'belden/open-perl-module)
	    (define-key map (kbd "s-a p c") 'cperl-check-syntax)
	    (define-key map (kbd "s-a p d") 'cperl-perldoc)
	    (define-key map (kbd "s-a p m") 'belden-cperl-mode)
	    (define-key map (kbd "s-a r n") '(lambda () (interactive) (random)))
	    (define-key map (kbd "s-a r i") '(lambda () (interactive) (random 2147483647)))
	    (define-key map (kbd "s-a r s") 'belden/random-string)
	    (define-key map (kbd "s-a s i") 'belden/shell-insert)
	    (define-key map (kbd "s-a t \"") 'belden/split-window-below/ansi-term)
	    (define-key map (kbd "s-a t %") 'belden/split-window-right/ansi-term)
	    (define-key map (kbd "s-a u b") 'belden/cperl-power-mode/update-buffers)

	    ;; more intrusive bindings
	    (define-key map (kbd "C-M-s") 'belden/findcode)
	    (define-key map (kbd "M-]") 'belden/goto-match-paren)
	    (define-key map (kbd "M-n") 'belden-toggle-hide-subs)

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

(defun cperl-deparse-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "perl -MO=Deparse"))

(defun cperl-concise-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "perl -MO=Concise"))

(defun belden/findcode  (findcode-command)
  "Run a findcode in separate buffer"
  (interactive
   (list (read-string "Run findcode as: "
		      (format "findcode %s" (_belden-current-keyword-or-quoted-active-region)))))
  (let ((compilation-buffer-name-function
	 (lambda (mode-name)
	   (format "*%s*" findcode-command))))
    (grep findcode-command)))

(defun belden/comparesub (comparesub-command)
  "View all definitions of a subroutine"
  (interactive
   (list (read-string "Run comparesub as: "
		      (format "comparesub %s" (_belden-current-keyword-or-quoted-active-region)))))
  (let ((compilation-buffer-name-function
	 (lambda (mode-name)
	   (format "*%s" comparesub-command))))
    (grep comparesub-command)))

(defun _belden-current-keyword-or-quoted-active-region ()
  (if mark-active (concat "'" (_belden-active-region) "'")
    (or (current-word nil nil) "")))

(defun _belden-active-region ()
  (buffer-substring (point) (mark)))

(defun belden/split-window-right/ansi-term ()
  "pop a new buffer right with an ansi-term in it"
  (interactive)
  (belden/split-window-right)
  (call-interactively (ansi-term "/bin/bash")))

(defun belden/split-window-below/ansi-term ()
  "pop a new buffer down with an ansi-term in it"
  (interactive)
  (belden/split-window-below)
  (call-interactively (ansi-term "/bin/bash")))

;; http://www.emacswiki.org/emacs/NavigatingParentheses
(defun belden/goto-match-paren (arg)
  "Go to the matching character: () {} [] <>

vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "[\[\{\<\(]") (forward-list 1) (backward-char 1))
	((looking-at "[\]\}\>\)]") (forward-char 1) (backward-list 1))))

(defun this-buffer-major-mode ()
  "return the major mode of the currently active buffer"
  (format "%s" (with-current-buffer (current-buffer) major-mode)))

(defun belden-toggle-hide-subs ()
  (interactive)
  (let ((funcstr "sub "))
    (if (string-match "lisp" (this-buffer-major-mode))
        (setq funcstr
	      "(def\\(un\\|var\\|group\\|alias\\|custom\\|const\\|subst\\|macro\\|face\\) "))
    (if (string-match "javascript" (this-buffer-major-mode))
	(setq funcstr "function")
      (if (string= comment-start "// ")
	  (setq funcstr
		"func")))
    (if line-move-ignore-invisible
        (progn (show-all-invisible) (setq line-move-ignore-invisible nil))
      (hide-non-matching-lines (format "^[\t ]*%s" funcstr))
      )))

(defun belden/align-to-fat-arrow (BEG END)
  "(align-regexp) to '=>'"
  (interactive "r")
  (align-regexp BEG END "\\(=>\\)" -1 0))

;; open-perl-file
(defun belden/open-perl-module (module)
  "open perl module"
  (interactive
   (list (let* ((default-entry (cperl-word-at-point))
		(input (read-string
			(format "open perl module%s: "
				(if (string= default-entry "")
				    ""
				  (format " (default %s)" default-entry))))))
	   (if (string= input "")
	       (if (string= default-entry "")
		   (error "no module name given")
		 default-entry)
	     input))))
  (belden-find-module module))

(defun belden/cperl-file-for-module (module)
  (concat (belden/root-dir) (cperl-module-name-to-filename module)))

(defun belden/root-dir ()
  "/home/dev/src/adama/lib/")

(defun cperl-module-name-to-filename (module)
  (replace-regexp-in-string ".pm.pm" ".pm"
  (replace-regexp-in-string "//" "/"
  (replace-regexp-in-string "::" "/"
  (replace-regexp-in-string "$" ".pm" module)))))

(defun belden-find-module (m)
  (if (consp current-prefix-arg)
      (other-window 1))
  (find-file (belden/cperl-file-for-module m)))

(defun belden/set-buffer-default-directory (dir)
  "set this buffer's notion of default directory"
  (interactive
   (list (let* ((default-entry (pwd))
		(input (read-string
			(format "set default directory%s: "
				(if (string= default-entry "")
				    ""
				  (format " (default %s)" default-entry))))))
	   (if (string= input "")
	       (if (string= default-entry "")
		   (error "no directory name given")
		 default-entry)
	     input))))
  (make-variable-buffer-local 'default-directory)
  (setq default-directory dir))

;; belden/follow
(defun belden-follow ()
  "Jump somewhere else based on what is under the point"
  (interactive)
  (cond
   ((point-on-filenamep) (find-file (current-filename)))
   ((point-on-modulep) (belden-find-module (current-module)))
   ))

(defun point-on-modulep ()
  (let ((module (current-module)))
    (and (string-match "::" module)
	 (not (string-match "^SUPER::" module)))))

(defun current-module ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:_")
    (let ((beg (point)) module)
      (skip-chars-forward "A-Za-z0-9:_")
      (setq module (buffer-substring beg (point)))
      module)))

(defun point-on-filenamep ()
  (let ((filename (thing-at-point 'filename)))
    (file-exists-p filename)))

(defun current-filename ()
  (thing-at-point 'filename))
