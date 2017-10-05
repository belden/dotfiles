(provide 'belden/loadpackages)
(require 'package)
(require 'cl)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar belden/required-packages
  '(afternoon-theme
    ample-theme
    ample-zen-theme
    clues-theme
    color-theme
    color-theme-solarized
    csharp-mode
    cycle-themes
    ember-mode
    ember-yasnippets
    yasnippet
    eruby-mode
    go-mode
    handlebars-mode
    hide-lines
    js2-mode
    json-mode
    json-reformat
    less-css-mode
    list-processes+
    magit
    magit-popup
    dash
    async
    git-commit
    with-editor
    mmm-mode
    nav
    org-jira
    rainbow-delimiters
    rainbow-identifiers
    rotate
    sos
    org
    sotlisp
    ace-jump-mode
    ace-jump-buffer
    ace-jump-zap
    ace-window
    ) "a list of packages to ensure are installed at launch.")

(defun belden/packages-installed-p ()
  (loop for p in belden/required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

; check that all packages are installed
(unless (belden/packages-installed-p)
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message " done.")
  (dolist (p belden/required-packages)
	 (when (not (package-installed-p p))
	   (package-install p))))

;; misplaced stuff from ~/temp.el
(defun belden-toggle-hide-subs2 ()
  (interactive)
  (let ((funcstr
	 (cond
	  ((string-match "lisp" (this-buffer-major-mode))
	   '("(def\\(un\\|var\\|group\\|alias\\|custom\\|const\\|subst\\|macro\\|face\\) " 1))
	  ((string-match "javascript" (this-buffer-major-mode))
	   '("function" 1))
	  ((string-match "sql" (this-buffer-major-mode))
	   '("\\(function\\|FUNCTION\\)") 0)
	  ((string= comment-start "// ")
	   '("func" 1))
	  (t '("sub " 1)))))
    (if line-move-ignore-invisible
        (progn
	  (show-all-invisible)
	  (setq line-move-ignore-invisible nil))
      (hide-non-matching-lines
       (cond
	((= 0 (car (cdr funcstr)))
	 (format "%s" (car funcstr)))
	(t (format "^[\t ]*%s" (car funcstr))))))))

;;; http://stackoverflow.com/a/6155151
(defun upcase-rectangle (b e)
  "change chars in rectangle to uppercase"
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line b e))
(defun upcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
		   (progn (move-to-column endcol 'coerce)
			  (point)))))


;(defadvice sql-postgres (before clear-perl5lib activate)
;  (make-local-variable 'process-environment)
;  (setenv "PERL5LIB" ""))

(defun adama-perl5lib ()
  "set PERL5LIB to something useful for Adama"
  (interactive)
  (setenv "PERL5LIB" (concat
		      "/home/dev/src/adama/lib:"
		      "/home/dev/src/adama/t/lib:"
		      "/home/dev/src/adama/extlib/lib/perl5/x86_64-linux-thread-multi:"
		      "/home/dev/src/adama/extlib/lib/perl5")))
(adama-perl5lib)

(defun line-like (re)
  (string-match re (buffer-substring (line-beginning-position) (line-end-position))))


(defun cperl-current-class-name ()
  "Generate a classname from the given buffer name"
  (interactive)
  (insert
    (replace-regexp-in-string "^.*Adama" "Adama"
    (replace-regexp-in-string "/" "::"
    (replace-regexp-in-string ".pm$" ""
    (buffer-file-name))))))

(defun belden-zap-to-char (arg char)
  "Like `zap-to-char' but doesn't delete CHAR"
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
			 (- (point) 1))))
