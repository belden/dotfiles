(provide 'belden-follow)

(defvar belden-follow:dirlist '(
				/home/dev/src/adama/lib/
				/home/dev/src/adama/t/lib/
				)
  "directories to scan when `belden-follow` looks for a file")


;; open-perl-file
(defun belden/cperl-open-module (module)
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
  (find-file (belden-follow:cperl-find-filename module)))


;; belden-follow
(defun belden-follow ()
  "Jump somewhere else based on what is under the point"
  (interactive)
  (cond
   ((point-on-filenamep) (find-file (current-filename)))
   ((point-on-modulep) (find-file (belden-follow:cperl-find-filename (current-module))))
   ))

(defun belden-grep (condp list)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) list)))

(defun belden-follow:cperl-find-filename (m)
  (let* ((needle (cperl-module-name-to-filename m))
	 (haystack (mapcar (lambda (x) (format "%s%s" x needle)) belden-follow:dirlist)))
    (car (belden-grep 'file-exists-p haystack))))

;; question-and-answers for belden-follow
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

;; cperl helper functions
(defun cperl-module-name-to-filename (module)
  (replace-regexp-in-string ".pm.pm" ".pm"
  (replace-regexp-in-string "//" "/"
  (replace-regexp-in-string "::" "/"
  (replace-regexp-in-string "$" ".pm" module)))))

