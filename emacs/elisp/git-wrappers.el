(provide 'git-wrappers)

(require 'belden-editing)

(defun git-fc (git-fc-command)
   "Run a git-fc in separate buffer"
   (interactive
    (list (read-string "Run git-fc as: "
                       (format "git --no-pager grep --full-name -n %s" (belden-current-keyword-or-quoted-active-region)))))
   (let ((compilation-buffer-name-function
          (lambda (mode-name)
            (format "*%s*" git-fc-command))))
     (grep (format "cd %s; %s | sed 's,^,%s/,'" (getenv "code_root") git-fc-command (getenv "code_root")))))
