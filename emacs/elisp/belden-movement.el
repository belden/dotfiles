(provide 'belden-movement)

(defun move-to-sub-definition ()
  (when (not (string-match "{" (current-line-prefix)))
    (beginning-of-line)
    (skip-chars-forward " ")
    (forward-char 5)
    (skip-chars-backward "A-Za-z0-9_")))

(defun belden-find-matching-position ()
  (save-excursion
    (let ((matchchar (current-char)) closechar backward pos)
      (when (string= matchchar "(") (setq closechar ")"))
      (when (string= matchchar ")") (setq closechar "(") (setq backward 't))
      (when (string= matchchar "[") (setq closechar "]"))
      (when (string= matchchar "]") (setq closechar "[") (setq backward 't))
      (when (string= matchchar "{") (setq closechar "}"))
      (when (string= matchchar "}") (setq closechar "{") (setq backward 't))
      (when (string= matchchar "<") (setq closechar ">"))
      (when (string= matchchar ">") (setq closechar "<") (setq backward 't))
      (if (not closechar) (error "Not on a blinkable char, try one of '(){}[]<>'"))
      (condition-case nil
          (belden-matching-char-position matchchar closechar backward)
        (error (error "Couldn't find matching '%s'." closechar))))))

(defun belden-matching-char-position (matchchar closechar backward)
  (save-excursion
    (let ((count 0) currchar pos
          (regexp (concat (regexp-quote matchchar) "\\|" (regexp-quote closechar)))
          (myface (face-at-point))
          )
      (while (not pos)
        (if backward
            (search-backward-regexp regexp)
          (progn
            (forward-char 1)
            (search-forward-regexp regexp)
            (backward-char 1)))
        (if (not (equal (face-at-point) myface)) 'nil
          (if (string= (current-char) matchchar)
              (setq count (1- count))
            (if (= count 0) (progn (setq pos (point)))
              (setq count (1+ count))))))
      pos
      )))

(defun safe-backward-char (distance)
  (if (<= (point) distance) (goto-char 0) (backward-char distance)))

(defun move-over-arrow ()
  (if (looking-at "->")
      (forward-char 2)
    (if (and (looking-at ">") (string-equal (previous-char) "-"))
        (forward-char 1))))

(defun belden-recenter-other-window ()
  "Recenter the other window, whatever that means"
  (interactive)
  (other-window 1)
  (recenter-top-bottom)
  (other-window 1))
