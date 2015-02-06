;; Monkey Patch diff-find-file-name to add a hook for finding files

(defvar diff-find-file-hooks nil)

(defun diff-run-find-file-hooks (fs)
  "Run each find file hook until one returns non-nil"
  (let ((found nil) (hooks diff-find-file-hooks))
    (while (and (not found) hooks)
      (setq found (funcall (car hooks) fs))
      (setq hooks (cdr hooks)))
    found))

(defun diff-find-file-name (&optional old noprompt prefix)
  "Return the file corresponding to the current patch.
Non-nil OLD means that we want the old file.
Non-nil NOPROMPT means to prefer returning nil than to prompt the user.
PREFIX is only used internally: don't use it."
  (unless (equal diff-remembered-defdir default-directory)
    ;; Flush diff-remembered-files-alist if the default-directory is changed.
    (set (make-local-variable 'diff-remembered-defdir) default-directory)
    (set (make-local-variable 'diff-remembered-files-alist) nil))
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let ((fs (diff-hunk-file-names old)))
      (if prefix (setq fs (mapcar (lambda (f) (concat prefix f)) fs)))
      (or
       ;; use any previously used preference
       (cdr (assoc fs diff-remembered-files-alist))
       ;; try to be clever and use previous choices as an inspiration
       (dolist (rf diff-remembered-files-alist)
	 (let ((newfile (diff-merge-strings (caar rf) (car fs) (cdr rf))))
	   (if (and newfile (file-exists-p newfile)) (return newfile))))
       ;; look for each file in turn.  If none found, try again but
       ;; ignoring the first level of directory, ...
       (do* ((files fs (delq nil (mapcar 'diff-filename-drop-dir files)))
	     (file nil nil))
	   ((or (null files)
		(setq file (do* ((files files (cdr files))
				 (file (car files) (car files)))
			       ;; Use file-regular-p to avoid
			       ;; /dev/null, directories, etc.
			       ((or (null file) (file-regular-p file))
				file))))
	    file))
       ;; <foo>.rej patches implicitly apply to <foo>
       (and (string-match "\\.rej\\'" (or buffer-file-name ""))
	    (let ((file (substring buffer-file-name 0 (match-beginning 0))))
	      (when (file-exists-p file) file)))
       ;; If we haven't found the file, maybe it's because we haven't paid
       ;; attention to the PCL-CVS hint.
       (and (not prefix)
	    (boundp 'cvs-pcl-cvs-dirchange-re)
	    (save-excursion
	      (re-search-backward cvs-pcl-cvs-dirchange-re nil t))
	    (diff-find-file-name old noprompt (match-string 1)))
       (diff-run-find-file-hooks (first fs))
       ;; if all else fails, ask the user
       (unless noprompt
         (let ((file (read-file-name (format "Use file %s: "
                                             (or (first fs) ""))
                                     nil (first fs) t (first fs))))
           (set (make-local-variable 'diff-remembered-files-alist)
                (cons (cons fs file) diff-remembered-files-alist))
           file))))))


(add-hook 'diff-find-file-hooks
          '(lambda (fs)
             (let ((new-file (concat (root "/" fs))))
               (if (file-exists-p new-file) new-file))))


(provide 'belden-diff-mode)
