(require 'cperl-mode)

(defun belden-indent-cperl-region-or-line ()
  (interactive)
  (belden-generic-indent-region-or-line (function cperl-indent-command)))

(defun belden-generic-indent-region-or-line (indent-command)
  (if mark-active
      (progn  ; set beg end to marked region
        (if (< (point) (mark)) (exchange-point-and-mark))
        (cperl-indent-region (mark) (1- (point)))
        )
    (let ((previous-key (previous-key)))
      (if (and (or (looking-at "\\>")
                   (empty-line-suffix)
                   (string-equal (previous-char) ":")
                   (eq previous-key 'left)
                   (eq previous-key 'right))
               (not (eq previous-key 'down))
               (not (eq previous-key 'up))
               (not (and (eolp) (string-match "[\]\)\}\,]" (previous-char))))
               (not (empty-line-prefix)))
          (hippie-expand 'nil)
        (progn
          (setq he-num -1)
         (funcall indent-command))))))

(defun belden-toggle-if-unless ()
  "Toggle modifiers between postfix and prefix"
  (interactive)
  (if (looking-at "(")
    (progn
      (cperl-invert-if-unless)
      (backward-char 1)
      (skip-chars-backward "A-Za-z\ ")
      )
    (belden-expand-if-unless)
  )
)

(defun belden-expand-if-unless ()
  "Convert postfix modifier to prefix form"
  (interactive)
  (let* (start beg indent end (default-indent-step 2) (postfix_p nil)
               chunk1 chunk2)
    (save-excursion
      (skip-chars-backward "A-Za-z")
      (setq start (point))
      (if (looking-at "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>")
          (setq postfix_p t)
        (error "Not on postfix while, if, for, foreach, until, unless" nil)
        )
      )
    (if postfix_p
        (progn
          (goto-char start)
          ; find beginning of block
          (if mark-active
              (progn
                (goto-char (mark))
                (skip-chars-forward " \t")
                (back-to-indentation)
                (setq beg (point)))
            (if (eq (current-column) ; two line $this if $that
                    (progn (back-to-indentation) (current-column)))
                (progn
                  (previous-line 1)
                  (back-to-indentation)
                  (setq beg (point))
                  )
              (progn
                (back-to-indentation)
                (setq beg (point))
                )
              )
            )
          ; find end of conditional
          (goto-char start) (end-of-line) (setq end (point))

          ; determine indentation
          (goto-char beg)
          (setq indent (+ (current-column) default-indent-step))

          ; kill both sections
          (goto-char start)
          (setq chunk1 (buffer-substring beg start))
          (setq chunk2 (buffer-substring start end))
          (delete-region start end)
          (delete-region beg start)

          ; bring back the 'if'
          (insert chunk2)

          ; remove trailing spaces and semis
          (end-of-line)
          (backward-char 1)
          (while (looking-at ";\\|\\ ") (delete-char 1) (backward-char 1))

          ; insert parens
          (back-to-indentation)
          (skip-chars-forward "A-Za-z_")
          (skip-chars-forward " ")
          (insert-char ?\050 1)
          (end-of-line)
          (insert-char ?\051 1)
          (insert-char ?\  1)
          (insert-char ?\{ 1)

          ; add indentation
          (newline)
          (insert-char ?\  indent)

          ; pull back in our block
          (insert chunk1)
          (backward-char 1)
          (while (looking-at ";\\|\\ \\|\n") (delete-char 1) (backward-char 1))
          (forward-char 1)
          (insert-char ?\; 1)

          ; close the block
          (newline)
          (insert-char ?\  (- indent default-indent-step))
          (insert-char ?\} 1)
          (setq end (point))

          ; go to the first paren
          (goto-char beg)
          (search-forward "(")
          (backward-char 1)
          (cperl-indent-region beg end)
          )
      )
    )
  )

(define-derived-mode belden-cperl-mode cperl-mode 
  "SS CPerl" 
  "modifications to cperl-mode"

  (define-key belden-cperl-mode-map "\t" 'belden-indent-cperl-region-or-line)
  (define-key belden-cperl-mode-map "\C-c\C-t" 'belden-toggle-if-unless)
  (define-key belden-cperl-mode-map "\C-m" 'newline-and-indent)
  (define-key belden-cperl-mode-map "\C-j" 'iswitchb-buffer)
  (define-key belden-cperl-mode-map (kbd "\C-x DEL") 'cperl-electric-backspace)
  (if (string-match "^perl5db.pl\$" (buffer-name)) 
      (fundamental-mode) ; prevent expensive parsing for this boring file
    (imenu-add-to-menubar "Imenu"))

  ;; Make hashmap, hashgrep, and ilk highlight
  (font-lock-add-keywords 'belden-cperl-mode 
      `(( ,(concat
            "\\(^\\|[^$@%&\\]\\)\\<\\("
            "\\(hash\\(apply\\|grep\\|map\\|partition\\|of\\|of_by\\|sort\\|_merge\\)\\)"
            "\\|apply\\|all\\|any\\|build_hash\\|deepgrep\\|firstmap\\|flatten_by\\|partition\\|unzip\\|missing_ok\\|hash_each_ok\\|each_ok\\|sanity_each_ok\\|sanity_missing_ok\\|unzip\\|lkeys\\|lvalues\\|first\\|say\\|assert\\|exception\\)\\>")
          2 'cperl-nonoverridable-face)
        ("\\(^\\|[^$@%&\\]\\)\\<\\(state\\)\\>"
          2 'font-lock-keyword-face)))
 
  (set (make-local-variable 'compile-rpf-command) 
       (belden-find-test-file (buffer-file-name)))
  (set (make-local-variable 'compile-apc-command) 
       (belden-find-test-file (buffer-file-name)))
  (set (make-local-variable 'parens-require-spaces) nil)
  (local-set-key [S-f1] 'cperl-perldoc)
  (modify-syntax-entry ?_ "w" cperl-mode-syntax-table)

  (defun cperl-calculate-indent  (&optional parse-data)
    "Override of cperl-calculate-indent for style
     Return appropriate indentation for current line as Perl code.
     In usual case returns an integer: the column to indent to.
     Returns nil if line starts inside a string, t if in a comment.

     Will not correct the indentation for labels, but will correct it for braces
     and closing parentheses and brackets."
    ;; This code is still a broken architecture: in some cases we need to
    ;; compensate for some modifications which `cperl-indent-line' will add later
    (save-excursion
      (let ((i (cperl-sniff-for-indent parse-data)) what p)
;;        (message "%s" i) ;; PMS useful debug point
        (cond
         ;;((or (null i) (eq i t) (numberp i))
         ;;  i)
         ((vectorp i)
          (setq what (assoc (elt i 0) cperl-indent-rules-alist))
          (cond
           (what (cadr what))		; Load from table
           ;;
           ;; Indenters for regular expressions with //x and qw()
           ;;
           ((eq 'REx-part2 (elt i 0)) ;; [self start] start of /REP in s//REP/x
            (goto-char (elt i 1))
            (condition-case nil        ; Use indentation of the 1st part
                (forward-sexp -1))
            (current-column))
           ((eq 'indentable (elt i 0))	; Indenter for REGEXP qw() etc
            (cond ;;; [indentable terminator start-pos is-block]
             ((eq 'terminator (elt i 1)) ; Lone terminator of "indentable string"
              (goto-char (elt i 2))      ; After opening parens	    
              (beginning-of-line)
              (skip-chars-forward " \t")
              (+ (or cperl-regexp-indent-step cperl-indent-level)
                 (current-column)))
             ((eq 'first-line (elt i 1)) ; [indentable first-line start-pos]
              (goto-char (elt i 2))
              (beginning-of-line)
              (skip-chars-forward " \t")
              (+ (or cperl-regexp-indent-step cperl-indent-level)
                 (current-column)))
             ((eq 'cont-line (elt i 1)); [indentable cont-line pos prev-pos first-char start-pos]
              ;; Indent as the level after closing parens
              (goto-char (elt i 2))     ; indent line
              (skip-chars-forward " \t)") ; Skip closing parens
              (setq p (point))
              (goto-char (elt i 3))    ; previous line
              (skip-chars-forward " \t)") ; Skip closing parens
              ;; Number of parens in between:
              (setq p (nth 0 (parse-partial-sexp (point) p))
                    what (elt i 4))	; First char on current line
              (goto-char (elt i 3))	; previous line
              (+ (* p (or cperl-regexp-indent-step cperl-indent-level))
                 (cond ((eq what ?\) )
                        (- cperl-close-paren-offset)) ; compensate
                       ((eq what ?\| )
                        (- (or cperl-regexp-indent-step cperl-indent-level)))
                       (t 0))
                 (if (eq (following-char) ?\| )
                     (or cperl-regexp-indent-step cperl-indent-level)
                   0)
                 (current-column)))
             (t
              (error "Unrecognized value of indent: %s" i))))
           ;;
           ;; Indenter for stuff at toplevel
           ;;
           ((eq 'toplevel (elt i 0)) ;; [toplevel start char-after state immed-after-block]
            (+ (save-excursion		; To beg-of-defun, or end of last sexp
                 (goto-char (elt i 1))	; start = Good place to start parsing
                 (- (current-indentation) ;
                    (if (elt i 4) cperl-indent-level 0))) ; immed-after-block
               (if (eq (elt i 2) ?{) cperl-continued-brace-offset 0) ; char-after
               ;; Look at previous line that's at column 0
               ;; to determine whether we are in top-level decls
               ;; or function's arg decls.  Set basic-indent accordingly.
               ;; Now add a little if this is a continuation line.
               (if (elt i 3)		; state (XXX What is the semantic???)
                   0
                 cperl-continued-statement-offset)))
           ;;
           ;; Indenter for stuff in "parentheses" (or brackets, braces-as-hash)
           ;;
           ((eq 'in-parens (elt i 0))
            ;; in-parens char-after old-indent-point is-brace containing-sexp

            ;; group is an expression, not a block:
            ;; indent to just after the surrounding open parens,
            ;; skip blanks if we do not close the expression.
            (progn 
              (goto-char (elt i 2))     ; old-indent-point
              (beginning-of-line)
              (skip-chars-forward " \t")
              (+ (current-column) 
                 cperl-indent-level
                 (cond
                  ((looking-at "if ") 2)
                  ((looking-at "unless ") 2)
                  ((looking-at "while ") 2)
                  (t 0)))))
           ;;
           ;; Indenter for continuation lines
           ;;
           ((eq 'continuation (elt i 0))
            ;; [continuation statement-start char-after is-block is-brace]
            (goto-char (elt i 1))       ; statement-start
            (if (looking-at "\\(hash\\)?\\(map\\|grep\\)")
                (current-column)
              (+ (if (memq (elt i 2) (append "}])" nil)) ; char-after
                     0                                   ; Closing parenth
                   cperl-continued-statement-offset)
                 (if (or (elt i 3)		; is-block
                         (not (elt i 4))        ; is-brace
                         (not (eq (elt i 2) ?\}))) ; char-after
                       0
                   ;; Now it is a hash reference
                   (+ cperl-indent-level cperl-close-paren-offset))
                 ;; Labels do not take :: ...
                 (if (looking-at "\\(\\w\\|_\\)+[ \t]*:")
                     (if (> (current-indentation) cperl-min-label-indent)
                         (- (current-indentation) cperl-label-offset)
                       ;; Do not move `parse-data', this should
                       ;; be quick anyway (this comment comes
                       ;; from different location):
                       (cperl-calculate-indent))
                   (current-column))
                 (if (eq (elt i 2) ?\{)	; char-after
                     cperl-continued-brace-offset 0))))
           ;;
           ;; Indenter for lines in a block which are not leading lines
           ;;
           ((eq 'have-prev-sibling (elt i 0))
            ;; [have-prev-sibling sibling-beg colon-line-end block-start]
            (goto-char (elt i 1))       ; sibling-beg
            (if (> (elt i 2) (point))   ; colon-line-end; have label before point
                (if (> (current-indentation)
                       cperl-min-label-indent)
                    (- (current-indentation) cperl-label-offset)
                  ;; Do not believe: `max' was involved in calculation of indent
                  (+ cperl-indent-level
                     (save-excursion
                       (goto-char (elt i 3)) ; block-start
                       (current-indentation))))
              (beginning-of-line)
              (skip-chars-forward " \t") 
              (current-column)))
           ;;
           ;; Indenter for the first line in a block
           ;;
           ((eq 'code-start-in-block (elt i 0))
            ;;[code-start-in-block before-brace char-after
            ;; is-a-HASH-ref brace-is-first-thing-on-a-line
            ;; group-starts-before-start-of-sub start-of-control-group]
            (goto-char (elt i 1))
            ;; For open brace in column zero, don't let statement
            ;; start there too.  If cperl-indent-level=0,
            ;; use cperl-brace-offset + cperl-continued-statement-offset instead.
            (+ (if (and (bolp) (zerop cperl-indent-level))
                   (+ cperl-brace-offset cperl-continued-statement-offset)
                 cperl-indent-level)
               (if (and (elt i 3)           ; is-a-HASH-ref
                        (eq (elt i 2) ?\})) ; char-after: End of a hash reference
                   (+ cperl-indent-level cperl-close-paren-offset)
                 0)
               ;; Unless openbrace is the first nonwhite thing on the line,
               ;; add the cperl-brace-imaginary-offset.
               (if (elt i 4) 0		; brace-is-first-thing-on-a-line
                 cperl-brace-imaginary-offset)
               (progn
                 (goto-char (elt i 6))	; start-of-control-group
                 (if (elt i 5)		; group-starts-before-start-of-sub
                     cperl-indent-level
                   ;; Get initial indentation of the line we are on.
                   ;; If line starts with label, calculate label indentation
                   (if (save-excursion
                         (beginning-of-line)
                         (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*:[^:]"))
                       (if (> (current-indentation) cperl-min-label-indent)
                           (- (current-indentation) cperl-label-offset)
                         ;; Do not move `parse-data', this should
                         ;; be quick anyway:
                         (cperl-calculate-indent))
                     (current-indentation))))))
           (t
            (error "Unrecognized value of indent: %s" i))))
         (t
          (error "Got strange value of indent: %s" i)))))))

(provide 'belden-cperl-mode)
