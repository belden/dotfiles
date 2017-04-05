;; begin <belden/movement-mode.el>
(provide 'belden/movement-mode)

(define-minor-mode belden/movement-mode
  "Add a bunch of movement bindings that Belden likes."
  :lighter " 李"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-W M-J") 'windmove-down)
	    (define-key map (kbd "M-W M-H") 'windmove-left)
	    (define-key map (kbd "M-W M-K") 'windmove-up)
	    (define-key map (kbd "M-W M-L") 'windmove-right)
	    (define-key map (kbd "C-x \"") 'belden/split-window-below)
	    (define-key map (kbd "C-x %") 'belden/split-window-right)
	    (define-key map (kbd "C-x 1") 'delete-other-windows-vertically)
	    (define-key map (kbd "C-j C-j") 'mode-line-other-buffer)
	    map))

(defun belden/split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun belden/split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))
;; end </belden/movement-mode.el>

;; begin <belden/cperl-power-mode.el>
(provide 'belden/cperl-power-mode)
(require 'rect)
(require 'magit)

(define-minor-mode belden/cperl-power-mode
  "Add a bunch of keybindings that I got used to at AirWave"
  :lighter " 登"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "s-a = >") 'belden/align-to-fat-arrow)
	    (define-key map (kbd "s-a c s") 'belden/comparesub)
	    (define-key map (kbd "s-a d b") 'cperl-insert-debug-breakpoint)
	    (define-key map (kbd "s-a d d") 'belden/set-buffer-default-directory)
	    (define-key map (kbd "s-a d w") 'delete-trailing-whitespace)
            (define-key map (kbd "s-a g s") 'magit-status)
            (define-key map (kbd "s-a g g") 'vc-git-grep)
            (define-key map (kbd "s-a f d") 'diff-buffer-with-file)
	    ;; (define-key map (kbd "s-a h ~") 'hide-lines-matching)
	    ;; (define-key map (kbd "s-a h !") 'hide-lines-not-matching)
	    ;; (define-key map (kbd "s-a h 0") 'hide-lines-show-all)

	    (define-key map (kbd "s-a m x") 'belden/cperl-power-mode/save-and-make-executable)
	    (define-key map (kbd "s-a p m") 'belden-cperl-mode)
	    (define-key map (kbd "s-a r d") 'rainbow-delimiters-mode)
	    (define-key map (kbd "s-a r n") '(lambda () (interactive) (insert (format "%s" (random 10000)))))
	    (define-key map (kbd "s-a r i") 'rainbow-identifiers-mode)
	    (define-key map (kbd "s-a r s") 'belden/random-string)
	    (define-key map (kbd "s-a r >") 'belden/shift-right)
	    (define-key map (kbd "s-a s i") 'belden/shell-insert)
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

;; (require 'hide-lines)
;; (defalias 'show-all-invisible 'hide-lines-show-all)
;; (defalias 'hide-matching-lines 'hide-lines-matching)
;; (defalias 'hide-non-matching-lines 'hide-lines-not-matching)

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
  (let* ((list (buffer-list))  (buffer (car list)) errmesg)
    (loop for buffer in (buffer-list) do
          (if (and (not (string-match "\\*" (buffer-name buffer)))
                   (buffer-file-name buffer)
                   (file-exists-p (buffer-file-name buffer)))
              (if (and (not (verify-visited-file-modtime buffer)) ; been touched
                       (buffer-modified-p buffer)) ; and modified
                  (setq errmesg (concat errmesg
					(format "Buffer '%s' has file and buffer changes!\n" buffer)))
                (belden/cperl-power-mode/update-buffer buffer))))
    (message "%s" (or errmesg "Done refreshing all open non-modified files...")))
  (magit-refresh))

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

(defun belden/comparesub (comparesub-command)
  "View all definitions of a subroutine"
  (interactive
   (list (read-string "Run comparesub as: "
		      (format "js-cs %s" (_belden-current-keyword-or-quoted-active-region)))))
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

(defun belden/shift-right (start end string)
  "indent active region right two spaces"
  (interactive
   (progn (list
	   (region-beginning)
	   (region-end)
	   "  ")))
  (funcall 'string-insert-rectangle start end string))
;; end </belden/cperl-power-mode.el>

;; maybe I'll regret this one day: make M-{h,j,k,l} be interpreted as {left,down,up,right}
(define-key input-decode-map (kbd "M-h") [left])
(define-key input-decode-map (kbd "M-j") [down])
(define-key input-decode-map (kbd "M-k") [up])
(define-key input-decode-map (kbd "M-l") [right])

;; M-H, M-J, M-K, M-L act as <back-word>, <4-down>, <4-up>, <forward-word>
(global-set-key (kbd "\eH") 'backward-word)
(global-set-key (kbd "\eJ") '(lambda () (interactive) (next-line 4)))
(global-set-key (kbd "\eK") '(lambda () (interactive) (previous-line 4)))
(global-set-key (kbd "\eL") 'forward-word)

;; make C-q send s-a
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q") nil)
(define-key function-key-map (kbd "C-q") nil)
(define-key function-key-map (kbd "C-q") (kbd "s-a"))

;; belden-save
(global-unset-key (kbd "C-x C-s"))
(global-set-key (kbd "C-x C-s") '(lambda () (interactive) (save-buffer) (save-some-buffers)))

;; turn things on
(belden/cperl-power-mode)
(belden/movement-mode)

;; font size adjustment
(global-unset-key (kbd "s--"))
(global-set-key (kbd "s--") 'spacemacs/scale-down-font)
(global-set-key (kbd "s-+") 'spacemacs/scale-up-font)
