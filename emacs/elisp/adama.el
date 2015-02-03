(provide 'adama-ide)

;;
;; stuff specific to developing on adama
;;

;; assumes you've cloned github.com/belden/dotfiles and have run `make` therein
(defun adama-devtool (tool &rest args)
  (concat (format "%s/bin/development-tools/%s" (getenv "HOME") tool) " " (list-join args)))

;; create a new schema change file
(defun adama-new-schema-change (change-name)
  "Create a new schema change file"
  (interactive
   (list (read-string "description of new schema change: ")))
  (let ((adama-schema-file (shell-command-to-string (adama-devtool "adama-create-new-schema-change" "--emacs" "--name" (format "'%s'" change-name)))))
    (if (file-exists-p adama-schema-file)
        (find-file adama-schema-file))))
