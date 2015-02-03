(provide 'belden-point-funcs)
(require 'belden-movement)
(require 'belden-navigation)

(defun filename-under-point ()  ;; parse out Foo/Bar.pm:20:
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:/_.\\-")
    (let ((beg (point)))
      (skip-chars-forward "A-Za-z0-9:/_.\\-")
      (buffer-substring beg (point)))))

; where is the cursor?
(defun point-on-subdefp ()
  (save-excursion
    (move-to-sub-definition)
    (skip-chars-backward " ")
    (safe-backward-char 3)
    (looking-at "sub ")))

(defun point-on-constantp ()
  (save-excursion
    (and (> (length (current-constant)) 6)
         (progn (skip-chars-backward "A-Za-z0-9_")
                (and (not (string= (previous-char) ":"))
                     (not (string= (previous-char) "/")))
                ))))

(defun point-on-modulep ()
  (let ((module (current-module)))
    (and (string-match "::" module)
         (not (string-match "^SUPER::" module)))))

(defun point-on-functionp ()
  (save-excursion
    (skip-chars-forward "A-Za-z0-9_\\-")
    (looking-at "\(")))

(defun point-on-variablep ()
  (save-excursion
    (if (looking-at "[@$%]") (forward-char 1))
    (skip-chars-backward "{A-Za-z0-9_")
    (safe-backward-char 1)
    (looking-at "[@$%]")))

(defun point-on-hash-keyp ()
  (save-excursion
    (search-forward " " (line-end-position) 't)
    (looking-at "=>")))

(defun point-on-qwsubp ()
  (save-excursion
    (let ((beg (point)))
      (and (progn
             (search-backward-regexp "\(" 'nil 't)
             (safe-backward-char 2)
             (looking-at "qw"))
           (progn
             (beginning-of-line)
             (looking-at "use "))
           (progn (search-forward-regexp "\)")
                  (> (point) beg))))))

(defun point-on-object-methodp ()
  (and (point-on-methodp)
       (save-excursion
         (move-over-arrow)
         (skip-chars-backward "A-Za-z0-9:_\\->")
         (safe-backward-char 1)
         (looking-at "\\$"))))

(defun point-on-methodp ()
  (save-excursion
    (move-over-arrow)
    (skip-chars-backward "A-Za-z0-9:_\\-")
    (safe-backward-char 2)
    (looking-at "->")))

(defun point-on-class-methodp ()
  (and (not (point-on-object-methodp)) (point-on-methodp)))

; where in the file am I?
(defun current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))
(defun current-line-full ()
  (buffer-substring (line-beginning-position) (+ 1 (line-end-position))))
(defun current-line-prefix ()
 (buffer-substring (line-beginning-position) (point)))
(defun current-line-suffix () (buffer-substring (point) (line-end-position)))
(defun current-line-number ()
  (let ((linenum (string-to-int (substring (what-line) 5))))
    (message "")
    linenum))
(defun current-number ()
  (save-excursion
    (let (beg)
      (skip-chars-backward "0-9")
      (setq beg (point))
      (skip-chars-forward "0-9")
      (buffer-substring beg (point)))))

; where on this line am I?
(defun next-char ()
  (if (>= (point) (1- (point-max)))
      (current-char)
    (char-to-string (char-after (1+ (point))))))
(defun current-char ()
  (char-to-string (following-char)))
(defun previous-char ()
  (char-to-string (preceding-char)))
(defun previous-string (&rest strlist)
  (let (found length)
    (loop for str in (flatten strlist) do
          (setq length (length str))
          (and (not found) (> length 0) (< length (point))
               (save-excursion
                 (backward-char length)
                 (when (looking-at str) (setq found str)))))
    found
    ))
(defun previous-key (&optional arg)
  (if (not arg) (setq arg 1))
  (let (recent-keys index)
    (setq recent-keys (recent-keys))
    (setq index (- (length recent-keys) (1+ arg)))
    (if (>= index 0)
        (aref recent-keys index)
      'nil)))
(defun previous-key-string (&optional arg)
  (belden-chord-for-key (vector (previous-key arg))))
