(provide 'belden-diffing)
(require 'belden-shell-funcs)
(require 'belden-buffers)

(defun belden-diff-viewer (buffername ignore-whitespace git-args build_git_diff_cmd)
  (let ((diff-args "") linenum)
    (save-some-buffers)
    (switch-to-buffer (get-buffer-create buffername))
    (setq linenum (current-line-number))
    (erase-buffer)

    ; actually get a diff
    (diff-mode)
    (if ignore-whitespace (setq diff-args " -w"))
    (shell-command (format "cd %s; %s" (root)
                           (funcall build_git_diff_cmd diff-args git-args))
                   buffername)

    ; show untracked files at the head of the diff
    (goto-char (point-min))
		(belden-shell-function-insert (format "cd %s; git status -s | perl -ne '/^\\?/ && print'" (root)))
    (switch-to-buffer buffername)

    ; diffstats are helpful
    ;; (belden-diffstat-viewer)
    ;; (goto-line linenum)
    ;;       (setq default-directory (root))
    ;;   (insert (format "Empty%s diff.\n"
    ;;                   (if (string= (gitproj) "belden") ""
    ;;                     (concat " " (gitproj)))))
      ))

(defun belden-diffstat-viewer
  (interactive)
  (shell-command (format "cd %s; git diff --stat=%s" (root) (window-width)) "*diffstat*"))

(defun belden-git-diff (&optional ignore-whitespace git-args)
  (interactive "P")
  (belden-diff-viewer "*git diff*" ignore-whitespace git-args 'belden-build-git-diff-cmd))

;; (defun belden-diff-start ()
;; 	(interactive)
;; 	(save-excursion
;; 		(let ((branch (belden-shell-function-eval (format "cd %s; git branch | grep '^*' | cut -b 3-" (root)))))
;; 			(belden-diff-viewer "*git start-diff*" 't "start-todo/31106-US6271-Revenue-monitoring todo/31106-US6271-Revenue-monitoring"))))

(defun belden-cached-git-diff ()
	(interactive)
	(belden-diff-viewer "*git diff*" 't "--cached" 'belden-build-git-diff-cmd))

(defun belden-build-git-diff-cmd (diff-args &optional git-args)
  (if (not diff-args) (setq diff-args ""))
  (if (not git-args) (setq git-args ""))
  ;; (format "git difftool %s -y -x 'diff -u --show-function-line=^[[:space:]]*sub[[:space:]] %s'" git-args diff-args))
  (format "%s" "git diff -w"))

(defun belden-apply-hunk (&optional revert)
  (interactive)
  (save-excursion
    (let ((filename (diff-find-file-name)) errormsg)
      (condition-case err (diff-apply-hunk revert)
        (error (setq errormsg (error-message-string err))))
      (belden-save-other-buffer (get-file-buffer filename))
      (when (and errormsg (not (string= errormsg "No next hunk")))
        (message errormsg)))))

(defun belden-revert-hunk ()
  (interactive)
  (belden-apply-hunk 't))

(defun belden-revert-file ()
  (interactive)
  (save-excursion
    (belden-walk-diff-hunks 'belden-revert-hunk)))

(defun belden-apply-file ()
  (interactive)
  (save-excursion
    (belden-walk-diff-hunks 'belden-apply-hunk)))

(defun belden-walk-diff-hunks (func)
  (let (beg)
    (beginning-of-line 1)
    (if (looking-at "diff --git ") (forward-line))
    (if (looking-at "index ") (forward-line))
    (search-backward-regexp "^index ")
    (forward-line)
    (setq beg (point))
    (condition-case nil (search-forward-regexp "^index ")
      (error (goto-char (point-max))))
    (while (> (point) beg)
      (progn
        (condition-case nil (search-backward-regexp "^@@ ")
          (error (goto-char (point-min))))
        (if (> (point) beg) (funcall func))))))

