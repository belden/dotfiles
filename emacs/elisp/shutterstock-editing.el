(provide 'belden-editing)

(defun belden-garbage-mode ()
	"Toggle line-highlight, line-numbering, and whitespace mode"
	(interactive)
	(global-hl-line-mode)
	(whitespace-mode)
	(global-linum-mode))

(defun belden-active-region ()
  (buffer-substring (point) (mark)))

(defun belden-current-keyword-or-quoted-active-region ()
  (if mark-active (concat "'" (belden-active-region) "'")
    (or (current-word nil t) "")))

(defun belden-ensure-trailing-comma ()
  "Add a trailing comma to the line if one is absent"
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (bobp)
      (backward-char 1)
      (cond
       ((looking-at ",")
        t)
       ((looking-at ";")
        (delete-char 1)
        (insert ","))
       (t
        (forward-char 1)
        (insert ",")))))
  (next-line 1))

(defun belden-comment-dwim ()
  (interactive)
  (if (not mark-active)
      (progn
        (if (eq last-command 'belden-comment-dwim)
            (kill-append (current-line-full) 'nil)
          (kill-new (current-line-full)))
        (comment-region (line-beginning-position) (line-end-position))
        (forward-line 1)
        )
    (comment-dwim nil)))

(defun belden-mark-whole-line ()
  (interactive)
  (push-mark (line-beginning-position))
  (forward-line 1)
  (beginning-of-line)
  (exchange-point-and-mark)
  (exchange-point-and-mark))

(defun belden-kill-whole-line ()
  (interactive)
  (belden-mark-whole-line)
  (kill-region (mark) (point)))

(defun belden-kill-whole-word ()
  "Kill the whole word"
  (interactive)
  (if mark-active
      (kill-region (mark) (point))
    (let* ((charset "A-Za-z") beg end)
      (setq non-charset (concat  "^" charset))
      (setq re-charset (concat  "[" charset "]"))
      (setq re-non-charset (concat  "[^" charset "]"))
      (backward-char 1)
      (if (looking-at "[^ \t\n][ \t\n][^ \t\n]")
          (forward-char 2)
        (progn 
          (forward-char 1)
          (if (looking-at "[ \t\n]")
              (fixup-whitespace)
            
            (if (looking-at re-non-charset)
                (kill-region 
                 (point) 
                 (progn (skip-chars-forward non-charset (line-end-position))
                        (point)))
              (progn
                
                (kill-region
                 (progn
                   (skip-chars-backward charset (line-beginning-position))
                   (point))
                 (progn
                   (skip-chars-forward  charset (line-end-position))
                   (point))
                 )))))))))

(defun belden-kill-line-or-region ()
  "Kill region if active, otherwise kill line"
  (interactive)
  (if mark-active (kill-region (mark) (point)) (kill-line)))


(defvar ss-list-mode)
(defvar ss-list-beg (make-marker))
(defvar ss-list-end (make-marker))
(defun belden-toggle-vertical-horizontal-list ()
  "Toggle perl lists from single-line horizontal to multiline vertical"
  (interactive)
  (if (or (not (interactive-p)) (not (equal this-command last-command)))
      (progn (belden-set-list-boundaries)
             (setq ss-list-mode (belden-pick-list-mode)))
    (if (equal ss-list-mode "one")
        (setq ss-list-mode "two")
      (if (equal ss-list-mode "two")
          (setq ss-list-mode "three")
        (if (equal ss-list-mode "three")
            (setq ss-list-mode "four")
          (if (equal ss-list-mode "four")
              (setq ss-list-mode "one")
          )))))
  (save-excursion
    (belden-backward-to-current-pblock)
    (if (equal ss-list-mode "one")
        (progn (message "Horizontal List") (belden-make-list-horizontal t))
      (if (equal ss-list-mode "two")
          (progn (message "Wrapped List") (belden-make-list-wrapped t))
        (if (equal ss-list-mode "three")
            (progn (message "Bare Wrapped List")
                   (belden-make-list-wrapped-bare t))
          (if (equal ss-list-mode "four")
            (progn (message "Vertical List") (belden-make-list-vertical t))
            ))))))

(defun belden-pick-list-mode ()
  (save-excursion
    (let (qw_p mode str)
      (goto-char (- ss-list-beg 2))
      (setq qw_p (looking-at "qw"))
      (if qw_p "three" ; bare
        (setq str (buffer-substring ss-list-beg ss-list-end))
        (if (string-match "[-=]>" str) "four" ; vertical
          "two"); wrapped
        ))))

(defun belden-set-list-boundaries ()
  (let ((info-list (current-pblock-boundaries)))
    (set-marker ss-list-beg (pop info-list))
    (set-marker ss-list-end (1+ (pop info-list)))))

(defun belden-make-list-horizontal (&optional preset-boundaries)
  "Condense a multiline perl list into a single line"
  (interactive)
  (save-excursion
   (if (not preset-boundaries) (belden-set-list-boundaries))
   (save-restriction
     (narrow-to-region ss-list-beg (1+ ss-list-end))

     ; fold it until there is only one line
     (goto-char ss-list-end)
     (while (> (count-lines (point-min) (point-max)) 1) (delete-indentation))

     ; remove optional trailing commas and spaces
     (goto-char (- ss-list-end 2))
     (while (looking-at "\\(,\\|\\ \\)") (delete-char 1) (backward-char 1))

     ; ensure proper spacing
     (goto-char ss-list-beg)
     (while (< (point) (1- ss-list-end))
       (forward-char 1)
       (if (looking-at " ")
           (just-one-space)
         (if (looking-at ",")
             (progn (forward-char 1) (just-one-space))
           (if (looking-at "=>")
               (progn (just-one-space)
                      (forward-char 2)
                      (just-one-space))))))
     ; remove leading and trailing space
     (goto-char (1+ ss-list-beg)) (delete-if-space)
     (goto-char (1- ss-list-end)) (delete-if-space)
   )))

(defun belden-make-list-wrapped (&optional preset-boundaries)
  "Wrap long list at 80 chars"
  (interactive)
  (if (not preset-boundaries) (belden-set-list-boundaries))
  (belden-make-list-vertical t "Wrapped"))

(defun belden-make-list-wrapped-bare (&optional preset-boundaries)
  "Wrap long list at 80 chars with a bare paren in front"
  (interactive)
  (if (not preset-boundaries) (belden-set-list-boundaries))
  (belden-make-list-vertical t "Wrapped" "Bare"))

(defun belden-make-list-vertical (&optional preset-boundaries wrapped bare)
  "Expand a single line perl list into a multiline one"
  (interactive)
  (if (not preset-boundaries) (belden-set-list-boundaries))
  (belden-make-list-horizontal t)
  (let* (qw_p indent prevpoint current-wrap-column (wrap-column 79)
             (default-indent-step 2))
    (save-excursion
      (save-restriction
        ; find current indentation
        (setq current-wrap-column (- wrap-column (current-column)))

        ; are we in a qw?
        (goto-char (- ss-list-beg 2))
        (setq qw_p (looking-at "qw"))

        ; determine indentation
        (back-to-indentation)
        (setq indent (+ (current-column) default-indent-step))

        ; narrow region
        (narrow-to-region ss-list-beg ss-list-end)

        ; remove optional trailing commas and spaces
        (goto-char (- ss-list-end 1))
        (skip-chars-backward " ,\n\t")
        (while (looking-at "\\(,\\|\\ \\)") (delete-char 1))

        ; indent first element
        (goto-char ss-list-beg)
        (skip-chars-forward "(\\|{")
        (while (looking-at "\\ \\|\n") (delete-char 1))
        (if (or bare (not wrapped))
            (progn
              (setq current-wrap-column wrap-column)
              (newline)
              (insert-char ?\  indent)))

        ; indent the rest of the elements
        (while (< (point) (- (point-max) 1))
          ; set marker for wrapping
         (if (and (not (looking-at "[ \t\n]*[\-\=]\>"))
                   (< (point) (- (point-max) 4)))
              (setq prevpoint (point)))

          ; move forward
          (forward-sexp 1)

          ; gobble white space
          (if (not wrapped)
              (while (looking-at "\\ \\|\n") (delete-char 1)))

          ; qw wrapping
          (if (and qw_p wrapped (> (current-column) current-wrap-column))
              (save-excursion
                (setq current-wrap-column wrap-column)
                (goto-char prevpoint)
                (while (looking-at "[ \t\n]") (delete-char 1))
                (newline)
                (insert-char ?\  indent)))
          ; skip over ,;
          (while (looking-at ",\\|;") (forward-char 1))

          ; skip over fat arrows
          (if (looking-at "[ \t\n]*\-\>") 'nil
            (if (looking-at "[ \t\n]*\=\>")
                (progn
                  (while (looking-at "[ \t\n]") (delete-char 1))
                  (insert-char ?\  1)
                  (forward-char 2)
                  (while (looking-at "[ \t\n]") (delete-char 1))
                  (insert-char ?\  1)
                  (backward-char 3)
                  )
              ; indent next element
              (if (not wrapped)
                  (progn
                    (while (looking-at "\\ \\|\n") (delete-char 1))
                    (newline)
                    (insert-char ?\  indent))
                (if (> (current-column) current-wrap-column)
                    ; indent prev element if wrapped and we're over limit
                    (save-excursion
                      (setq current-wrap-column wrap-column)
                      (goto-char prevpoint)
                      (while (looking-at "\\ \\|\n") (delete-char 1))
                      (newline)
                      (insert-char ?\  indent))))))
           ); end of while

          ; wrap last element of list if its over limit
          (if (> (current-column) current-wrap-column)
              (save-excursion
                (setq current-wrap-column wrap-column)
                (goto-char prevpoint)
                (newline)
                (insert-char ?\  indent)))
          ; indent closing paren on bare wrapped list
          (if (or bare (> (current-column) (- current-wrap-column 1)))
              (progn (if (not qw_p) (insert-char ?\, 1))
                     (newline)
                     (insert-char ?\  (- indent default-indent-step))))

          ; line up closing paren
          (if (not wrapped) (backward-delete-if-space default-indent-step))

          ; add in trailing comma
          (if (and (not qw_p) (not wrapped))
              (progn
                (forward-line -1)
                (end-of-line)
                (while (string-equal (previous-char) ",")
                  (backward-delete-char 1))
                (insert-char ?\, 1)))))))

(defun delete-if-space (&optional arg)
  (if (not arg) (setq arg 1))
  (dotimes (i arg) (if (looking-at "[ \t\n\r]") (delete-char 1))))

(defun backward-delete-if-space (&optional arg)
  (if (not arg) (setq arg 1))
  (dotimes (i arg)
    (if (string-equal (previous-char) " ") (backward-delete-char 1))))

(defun copy-from-above-or-below (&optional arg below)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point."
  (interactive "P")
  (let ((cc (current-column)) n (string "") wordlen)
    (save-excursion
      (if below (progn (end-of-line) (skip-chars-forward "\ \t\n"))
        (progn (beginning-of-line) (skip-chars-backward "\ \t\n")))
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (save-excursion
        (let ((start (point)))
          (forward-word 1)
          (setq wordlen (- (point) start))))
      (setq n (if arg wordlen (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (= (preceding-char) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\ ))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (save-excursion (end-of-line) (point))
				 (+ n (point)))))))
    (insert string)))
