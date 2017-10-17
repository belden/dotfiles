(provide 'belden-func)
(require 'belden-point-funcs)
(require 'belden-perl-introspection)
(require 'cperl-mode)
(require 'ansi-color)
(require 'string)

(defun direct-command-to-string (command &rest args)
  "Execute command COMMAND (bypassing shell) and return its output as a string."
  (with-output-to-string
    (with-current-buffer
      standard-output
      (apply 'call-process command nil t nil (flatten args)))))

(defun flatten (l)
  (cond
   ((null l) nil)
   ((atom (car l)) (cons (car l) (flatten (cdr l))))
   (t (append (flatten (car l)) (flatten (cdr l))))))

(defun shuttershutter (&rest path)
  (concat (home "/code") (apply 'concat path)))
(defun root (&rest path)
  (concat (getenv "code_root") (apply 'concat path)))
(defun shutterroot (&rest path)
  (concat (getenv "code_root") (apply 'concat path)))
(defun home (&rest path)
  (concat (getenv "HOME") (apply 'concat path)))

(defun belden-keyboard-quit ()
  (interactive)
  (when (not mark-active) (widen))
  (keyboard-quit))

(defun set-mark-and-goto-line (line)
   "Set mark and prompt for a line to go to."
   (interactive "NGoto Line: ")
   (push-mark nil t nil)
   (goto-line line))

(defun belden-readable-testoutput ()
  (interactive)
  (remove-text-properties (point-min) (point-max)
                          '(mouse-face highlight help-echo nil))
  (replace-string "\\n" "\012" 'nil (point-min) (point-max))
  (replace-string "\\t" "    " 'nil (point-min) (point-max))
  (replace-string "\\\\" "" 'nil (point-min) (point-max))
  (replace-string "\\cJ" "\012" 'nil (point-min) (point-max))
  (replace-string "\\\"" "\"" 'nil (point-min) (point-max)))

(defun belden-toggle-hide-subs ()
  (interactive)
  (let ((funcstr "sub "))
    (if (string-match "lisp" (this-buffer-major-mode))
        (setq funcstr
        "(def\\(un\\|var\\|group\\|alias\\|custom\\|const\\|subst\\|macro\\|face\\) "))
    (if (string-match "javascript\\|js-mode" (this-buffer-major-mode))
  (setq funcstr "function\\|[^\\\s]([^)]*)\\ *{")
      (if (string= comment-start "// ")
    (setq funcstr
    "func")))
    (if line-move-ignore-invisible
        (progn (show-all-invisible) (setq line-move-ignore-invisible nil))
      (hide-non-matching-lines (format "[\t ]*%s" funcstr))
      )))

(defun belden-isearch-star-triggers-regex-mode ()
  (interactive)
  (if (not isearch-regexp) (isearch-toggle-regexp))
  (isearch-process-search-char (string-to-char "*")))

(defun belden-files-defining-function (function)
  (setq str (shell-command-to-string
             (format (concat "find %s -type f -not -name '.*' -not -name '*~' "
                             "! -path '*/.git/*' "
                             "| xargs grep -l 'sub %s '")
                     (shuttershutter "/lib")
                     function)))
  (if (string= str "") (error "Couldn't find any subs called %s" function))
  (string-split "\n" (string-replace-match "\n\$" str "")))

(defun belden-file-exists-p (file)
  "True for files, false for directories"
  (and (file-exists-p file) (not (car (file-attributes file)))))

(defun belden-count-lines (beg end)
  (let (tmp)
    (if (< end beg) (progn (setq tmp beg) (setq beg end) (setq end tmp)))
    (save-excursion 
      (goto-char beg) (setq beg (line-beginning-position))
      (goto-char end) (setq end (line-beginning-position))
      )
    (count-lines beg end)))

(defun belden-iswitchb-otherwindow ()
  (interactive)
  "Toggle new buffer in otherwindow setting"
  (let ((buffer  (car iswitchb-matches)))
    (if (not (eq iswitchb-method 'otherwindow))
        (progn
          (message "Other window: %s" buffer)
          (setq iswitchb-method 'otherwindow))
      (progn (message "Same window:  %s" buffer)
             (setq iswitchb-method 'always-frame)))))

(defun belden-push-mark ()
  (interactive)
  (while (and global-mark-ring
	   (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      (pop global-mark-ring))
  (push-mark nil t nil))

;;;;;;;;;;;;;;;;;; belden-follow (jump based on text near point)
(require 'find-func)
(defun belden-follow ()
  "Jump somewhere else based on what is under the point, module or file"
  (interactive)
  (belden-push-mark)
  (let (diff-buffer (buffer (current-buffer)))
    (cond
     ((point-on-subdefp) (belden-find-supersub))
     ((point-on-object-methodp) (belden-find-object-method))
     ((point-on-methodp) (belden-find-method))
     ((point-on-modulep) (belden-find-module))
     ((point-on-qwsubp) (belden-find-qwsub))
     ((point-on-variablep) (belden-find-variable))
     ((point-on-functionp) (belden-find-function))
     ((function-called-at-point) (find-function-at-point))
     ((belden-find-file 1) 'nil )
		 ((filename-under-point) (find-file-at-point))
     (t (belden-find-function))
		 )
    (belden-set-diff-buffer diff-buffer)
    ))

(defun belden-find-variable ()
  (let ((variable (current-variable)) quotevar (skip t) (beg (point)))
    (setq quotevar (regexp-quote variable))
    (if (string-match "^\\\\\\$" quotevar)
        (setq quotevar (string-replace-match "^\\\\\\$" quotevar "[@$%]")))
    (end-of-line)
    (while skip
      (search-backward-regexp (concat "[^[:word:]]"
                                      "\\(my\\|our\\|local\\) [^=;\n]*"
                                      quotevar
                                      "[^[:word:]]"))
      (setq skip 'nil)
      (forward-char 1)
      (if (looking-at "\\(my\\|our\\|local\\) *\\([@$%][A-Za-z0-9_]+\\)")
          (if (not (string-match
                    (format "^%s$" quotevar)
                    (buffer-substring (match-beginning 2) (match-end 2))))
              (setq skip t)))
      (if (looking-at "\\(my\\|our\\|local\\) *[^(@$% ]") (setq skip t))
      )
    (search-forward-regexp quotevar)
    (skip-chars-backward "A-Za-z0-9_")
    (backward-char 1)

    (if (and (eq beg (point)) (string= (previous-key-string) "<f4>"))
        (progn (set-mark-command t) (set-mark-command t)))
    ))

(defun belden-find-method ()
  (move-over-arrow)
  (let ((info (current-method-call)) class method)
    (setq class (pop info))
    (setq method (pop info))
    (if (or (string= "" class) (downcasep class))
        (belden-find-object-method)
      (belden-find-module-with-sub (current-method)))))

(defun belden-find-object-method ()
  (move-over-arrow)
  (let ((info (current-method-call)) obj method class default-class)
    (setq obj (pop info))
    (setq method (pop info))
    (setq default-class (buffer-file-module))
    (if (string-match "^SUPER::" method)
        (progn
          (setq default-class (concat default-class "::SUPER"))
          (setq method (string-replace-match "^SUPER::" method ""))))
    (setq class (read-from-minibuffer
                 (if (string-equal "" obj)
                     (format "Class for ->%s: " method)
                   (format "Class for $%s->%s: " obj method))
                 (if default-class
                     (cons default-class
                           (+ (string-match "::" default-class) 3))
                   'nil)
                 perlclass-minibuffer-map
                 'nil
                 'perlclass-history))
    (belden-find-module-with-sub (format "%s->%s" class method))))

(defun belden-set-diff-buffer (buffer)
  (if buffer
      (progn
        (make-local-variable 'belden-diff-buffer)
        (setq belden-diff-buffer buffer))))

(defun belden-find-function ()
  (if (not (belden-goto-method-in-module 'nil (current-function)))
      (belden-find-function-super)))

(defun belden-search-forward-but-not-past (findregex dontpassregex)
  (let ((beg (point)) found)
    (when (search-forward-regexp findregex 'nil 't)
      (if (string-match dontpassregex (buffer-substring beg (point)))
          (goto-char beg)
        (setq found 't)))
    found))

(defun belden-goto-method-in-module (module method)
  (if module (belden-find-module module))
  (let (delegator (beg (point)) (found t))
    (save-excursion
      (if (not (looking-at "package ")) (goto-char (point-min)))
      (if (search-forward-regexp
           (format "sub[ \t\n\r]+%s[ \t\n\r\(]+.*{" method) 'nil 't)
          (setq beg (line-beginning-position))
        (setq found 'nil)
        ))
    (when (not found)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp "Class::Delegator" 'nil 't)
          (when (belden-search-forward-but-not-past (format "\\b%s\\b" method) ";")
            (setq beg (line-beginning-position))
            (setq found 't)
            (setq delegator 't)
            ))))
    (goto-char beg)
    (if delegator (recenter 'nil) (recenter 1))
    found))

(defun last-element (list)
  (if (listp list) (car (last list)) list))

(defun downcasep (string)
  (let ((char (car (string-to-list string))))
    (and (string-match "^[A-Za-z]" string) (eq char (downcase char)))
    ))
(defun upcasep (string)
  (let ((char (car (string-to-list string))))
    (and (string-match "^[A-Za-z]" string) (eq char (upcase char)))
    ))
(defun list-join (list &optional join-str)
  "Joins string LIST with JOIN-STR, whic defaults to space."
  (let* (ret
	 (ch  (or join-str " ")))
    (while list
      (setq ret (concat (or ret "") (car list)))
      (setq list (cdr list))
      (if list				;only if still elements
	  (setq ret (concat ret ch))))
    ret))
(defun starts-with (haystack needle)
  "Returns true if the second argument is a prefix of the first."
  (and (>= (length haystack) (length needle))
       (string= needle (substring haystack 0 (length needle)))))

(defun belden-find-function-super ()
  (let (files function exported)
    (setq function (current-function))
    (setq files (belden-files-defining-function function))
    (if (= (length files) 1)
        (progn
          (find-file (last-element files))
          (message "Found in: %s" (belden-short-filename))
          (belden-goto-method-in-module 'nil function))
      (progn
        (setq exported (belden-exported-function function))
        (if (and (stringp exported) (string= "NONE" exported))
            (message "%s" (list-join (sort (mapcar 'belden-shorten-filename files)
                                           'string-lessp) "\n"))
          (belden-goto-method-in-module (car exported) function))))))

(defun belden-short-filename ()
  (if (buffer-file-name)
      (belden-shorten-filename (buffer-file-name))))

(defun belden-shorten-filename (filename)
  (let
    ((base (shuttershutter)))
    ((home (home)))
  (or
   (string-replace-match
    (format "^%s%s" base "/lib/Shutterstock/Daemon/") filename "S:D/")
   (string-replace-match
    (format "^%s%s" base "/lib/Shutterstock/Payment/") filename "S:P/")
   (string-replace-match
    (format "^%s%s" base "/lib/Shutterstock/Submitter/") filename "S:S/")
   (string-replace-match
    (format "^%s%s" base "/lib/Shutterstock/Test/") filename "S:T/")
   (string-replace-match
    (format "^/%s/" home) filename "~/")
   )))

(defun whichpm (str) (direct-command-to-string (concat (getenv "HOME") "/bin/development-tools/whichpm") "--nonewline" str "--root" (buffer-file-name)))

(defun belden-find-module (&optional mod-string)
  "Switch buffer from Module::Foo::Bar::baz Module/Foo/Bar.pm"
  (interactive)
  (if (not mod-string) (setq mod-string (current-module-with-optional-method)))
  (if (not (belden-find-inlined-module mod-string))
      (if (string-match "->" mod-string)
          (belden-find-module-with-sub mod-string)
        (progn
          (let ((new-filename (whichpm mod-string)))
            (if (file-exists-p new-filename)
                (progn (find-file new-filename) (message mod-string))
              (belden-find-module-with-sub mod-string)))))))

(defun current-module-with-optional-method ()
  (let ((module (current-module)))
    (save-excursion
      (skip-chars-forward "A-Za-z0-9:_")
      (when (looking-at " -+> ")
        (skip-chars-forward " ->")
        (setq module (concat module "->" (current-word)))))
    module))

(defun belden-find-inlined-module (mod-string)
  (let (line-char)
    (if (string= mod-string "") 'nil
      (if (setq line-char
                (first-line-matching (format "package.*%s;" mod-string)))
          (if (< (belden-count-lines (point-min) line-char) 10) 'nil
            (progn
              (goto-char line-char)
              (back-to-indentation)
              't))
        'nil
        ))))

(defun first-line-matching (regexp)
  (let ((line-char 'nil))
    (save-excursion
      (goto-char 0)
      (if (search-forward-regexp regexp 'nil 't)
          (setq line-char (line-beginning-position))
        )
      line-char
      )
    ))

(defun belden-find-module-with-sub (mod-string)
  (let ((parsed (parse-method mod-string)) module function)
    (setq module (pop parsed))
    (setq function (pop parsed))
    (if (not function) (error "Can't find Module: %s" module))
    (if (or (not module) (string= module ""))
            (error "Can't find Module: %s" function))
    (if (and (not (string-match "^SUPER::" function)) (belden-goto-method-in-module module function))
        (message "%s->%s" module function)
      (belden-find-method-definition module function))))

(defun belden-find-supersub ()
  (save-excursion
    (let (beg sub (module (buffer-file-module)))
      (move-to-sub-definition)
      (setq sub (subdef-name-im-on))
      (if (not (belden-find-module-with-sub
                (format "%s->%s" (belden-perl-superclass) sub)))
          (progn
            (belden-find-module module)
            (message "Could not find %s"
                     (format "%s->SUPER::%s" module sub)))))))

(defun belden-find-file (&optional quiet new-filename)
  "Switch buffer from Module/Foo/Bar.pm:491: Module/Foo/Bar.pm line 491"
  (interactive)
  (let ((linenum 7) beg)
    (if (not new-filename) (setq new-filename (filename-under-point)))
    (setq filedata (append (split-string new-filename ":") '("-1")))
    (setq new-filename (pop filedata))
    (setq linenum (string-to-int (pop filedata)))
    (when (eq linenum -1)
      (save-excursion
        (search-forward " " nil t)
        (when (or (looking-at "line ")
                  (looking-at "at line "))
          (when (looking-at "at ") (forward-char 3))
          (forward-char 5)
          (setq beg (point))
          (skip-chars-forward "0-9")
          (when (not (eq (point) beg))
            (setq linenum (string-to-int (buffer-substring beg (point))))))))
    (setq dirs (cons default-directory (list
      (root "/lib")
      (root "/t/lib")
      (root "")
      (home)
      ""
    )))
    (let (found)
      (if (not (dolist (dir dirs found)
            (setq full-filename (format "%s/%s" dir new-filename))
            (if (string-match "^//" full-filename)
                (setq full-filename (string-replace-match "^//" full-filename "/")))
            (if (and (belden-file-exists-p full-filename) (not found))
                (progn
                  (setq found t)
                  (find-file full-filename)
                  (if (not (equal linenum -1)) (goto-line linenum)))
              )
            ))
          (if (belden-find-possible-module new-filename) 't
            (progn
              (or quiet (error "File doesn't exist: %s" new-filename))
              'nil))
        found))))

(defun belden-find-possible-module (module)
  (condition-case nil
      (belden-find-module module)
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;
(defun belden-find-test-file (filename)
  (if (and filename (string-match "\\.pm\$" filename))
      (belden-code-test-map filename)
    (if (and filename
             (string-match "\\.rb\$" filename)
             (not (file-executable-p filename))
             )
        (belden-code-test-map filename))
    filename))

(defvar belden-code-test-hash-table (make-hash-table :test 'equal))
(defun belden-add-code-test-hash-item (key value)
  (let ((key-expand (substitute-in-file-name key))
        (value-expand (substitute-in-file-name value)))
    (puthash key-expand value-expand belden-code-test-hash-table)
    (puthash value-expand key-expand belden-code-test-hash-table)
  ))
(defun belden-paired-files (file-pairs)
  (loop for keyvalue in file-pairs do
        (let ((key (car keyvalue)) (value (car (cdr keyvalue))))
          (belden-add-code-test-hash-item key value))))

(defun belden-expand-source-controlled-filename (filename)
  (expand-file-name filename (root)))
(defun belden-contract-source-controlled-filename (filename)
  (let ((newfile (substitute-in-file-name filename)))
    (if (starts-with newfile (root))
        (setq newfile (file-relative-name newfile (root)))
      )
    newfile
    )
  )

(defun belden-code-test-map (filename)
  (or (and (gethash (belden-contract-source-controlled-filename filename)
                     belden-code-test-hash-table)
           (belden-expand-source-controlled-filename
            (gethash (belden-contract-source-controlled-filename filename)
                     belden-code-test-hash-table)))
      (string-replace-match "\\.pm$" filename ".t")
      (string-replace-match "\\.t$"  filename ".pm")
      filename
      )
  )

(defun belden-toggle-code-test-buffer ()
  "Switch buffer from Blah.t to Blah.pm and vice versa"
  (interactive)
  (let ((new-filename (belden-code-test-map (buffer-file-name))))
    (if (file-exists-p new-filename)
        (find-file new-filename)
      (error "File doesn't exist %s" new-filename))))

;;;;;;;
(fset 'belden-perlclass-complete
      (make-hippie-expand-function '(belden-try-expand-perlclass)))
(defvar perlclass-history nil
  "History list for commands asking for a perl class.")
(defvar perlclass-minibuffer-map (copy-keymap minibuffer-local-map))
(define-key perlclass-minibuffer-map [tab] 'belden-perlclass-complete)

(defun belden-try-expand-perlclass (old)
  (when (not old)
    (he-init-string (he-perlclass-beg) (he-perlclass-end))
    (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list (belden-generate-perlclass-list)))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (if old (he-reset-string)) 'nil)
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun belden-generate-perlclass-list ()
  (let (str name-part dir-part dir-list class-list class-prefix class)
   (setq str (buffer-substring-no-properties (he-perlclass-beg) (point)))
   (and (not (string-equal str ""))
        (progn
          (if (string-match "[^:]:\$" str) (setq str (format "%s:" str)))
          (setq dir-part (reverse (string-split "::" str)))
          (setq name-part (pop dir-part))
          (setq dir-part (reverse dir-part))
          (setq class-prefix (list-join dir-part "::"))
          (setq dir-part (list-join dir-part "/"))
          (if (not dir-part) (setq dir-part ""))
          (setq dir-part (mercmerc (format "/lib/perl/%s" dir-part)))
          (setq dir-list
                (sort (file-name-all-completions name-part dir-part)
                      'string-lessp))
          ;; (message "'%s'" (list-join dir-list "\n"))
          (loop for class in dir-list do
                (if (and (not (string-match "^\\." class))
                         (or (< (length dir-list) 3)
                             (not
                              (string-match "^\\(CVS/\\|AMPRoot\\.pm\\)\$"
                                            class))))
                    (progn
                      (setq class (or
                                   (string-replace-match "\\.pm$" class "")
                                   (string-replace-match "\\/$" class "::"))
                            )
                      (if class (push
                                 (if class-prefix
                                     (format "%s::%s" class-prefix class)
                                   class)
                                 class-list)))))
          (setq class-list (reverse class-list))
          ;; (message "%s" (list-join class-list "\n"))
          class-list))))
