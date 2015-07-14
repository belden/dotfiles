(provide 'belden/cperl-power-mode)
(require 'sos)
(require 'belden-follow)
(require 'rainbow-delimiters)
(require 'rainbow-identifiers)

(define-minor-mode belden/cperl-power-mode
  "Add a bunch of keybindings that I got used to at AirWave"
  :lighter " 登"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "s-a = >") 'belden/align-to-fat-arrow)
	    (define-key map (kbd "s-a c c") 'cperl-current-class-name)
	    (define-key map (kbd "s-a c s") 'belden/comparesub)
	    (define-key map (kbd "s-a d b") 'cperl-insert-debug-breakpoint)
	    (define-key map (kbd "s-a d d") 'belden/set-buffer-default-directory)
	    (define-key map (kbd "s-a d m") 'belden/debug-this-method)
	    (define-key map (kbd "s-a d w") 'delete-trailing-whitespace)
	    (define-key map (kbd "s-a h ~") 'hide-lines-matching)
	    (define-key map (kbd "s-a h !") 'hide-lines-not-matching)
	    (define-key map (kbd "s-a h 0") 'hide-lines-show-all)

	    ;; understand more about the code you're looking at, using B:: modules
	    (define-key map (kbd "s-a B c") 'cperl-concise-region)
	    (define-key map (kbd "s-a B d") 'cperl-deparse-region)

	    (define-key map (kbd "s-a m x") 'belden/cperl-power-mode/save-and-make-executable)
	    (define-key map (kbd "s-a o p") 'belden/cperl-open-module)
	    (define-key map (kbd "s-a p c") 'cperl-check-syntax)
	    (define-key map (kbd "s-a p d") 'cperl-perldoc)
	    (define-key map (kbd "s-a p m") 'belden-cperl-mode)
	    (define-key map (kbd "s-a r d") 'rainbow-delimiters-mode)
	    (define-key map (kbd "s-a r n") '(lambda () (interactive) (insert (format "%s" (random 10000)))))
	    (define-key map (kbd "s-a r i") 'rainbow-identifiers-mode)
	    (define-key map (kbd "s-a r s") 'belden/random-string)
	    (define-key map (kbd "s-a s i") 'belden/shell-insert)
	    (define-key map (kbd "s-a s o") 'sos)
	    (define-key map (kbd "s-a u b") 'belden/cperl-power-mode/update-buffers)

	    ;; tmux-style bindings
	    (define-key map (kbd "s-a t \"") 'belden/split-window-below/ansi-term)
	    (define-key map (kbd "s-a t %") 'belden/split-window-right/ansi-term)
	    (define-key map (kbd "s-a t a") 'belden/test-all-methods-yo)
	    (define-key map (kbd "s-a t m") 'belden/test-this-method)
	    (define-key map (kbd "s-a r 1") 'rotate:main-horizontal)
	    (define-key map (kbd "s-a r 2") 'rotate:main-vertical)
	    (define-key map (kbd "s-a r }") 'rotate-window)
	    (define-key map (kbd "s-a r {") 'rotate-window)
	    (define-key map (kbd "s-a r o") 'rotate-layout)

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

(defun belden/test-this-method (method)
  "Just test this method"
  (interactive
   (list (read-string "method: "
		      (_belden-current-keyword-or-quoted-active-region))))
  (setenv "TEST_METHOD" method))

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
      (hide-non-matching-lines (format "[\t ]*%s" funcstr))
      )))


(defun belden/align-to-fat-arrow (BEG END)
  "(align-regexp) to '=>'"
  (interactive "r")
  (align-regexp BEG END "\\(=>\\)" -1 0))

(defun cperl-insert-debug-breakpoint ()
  "insert a debug break point"
  (interactive)
  (insert (concat "local $MY::var = 1; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
		  "$DB::single = 1 if $MY::var; 1; 1; # XXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")))

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

(defun belden/test-all-methods-yo ()
  (interactive)
  "unset TEST_METHOD environment variable"
  (setenv "TEST_METHOD" nil)
  (message "testing everything now, yo"))

(defun cperl-current-class-name ()
  "Generate a classname from the given buffer name"
  (interactive)
  (insert
   (replace-regexp-in-string "^.*Adama" "Adama"
   (replace-regexp-in-string "/" "::"
   (replace-regexp-in-string ".pm$" ""
   (buffer-file-name))))))
