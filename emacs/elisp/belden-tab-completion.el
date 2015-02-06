(provide 'belden-tab-completion)

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

(defun belden-try-expand-hashitems (old)
  (let (wordsize)
    (if (not old)
        (progn
          (if (gethash (buffer-substring (- (point) 4) (point)) belden-tab-completions)
              (setq wordsize 4)
            (if (gethash (buffer-substring (- (point) 3) (point)) belden-tab-completions)
                (setq wordsize 3)
              (if (gethash (buffer-substring (- (point) 2) (point)) belden-tab-completions)
                  (setq wordsize 2)
                (setq wordsize 1))))
          (he-init-string (- (point) wordsize) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (let ((item (gethash he-search-string belden-tab-completions)))
            (setq he-expand-list
                  (cond
                   ((functionp item) (list (funcall item)))
                   (t item))))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (if old (he-reset-string)) 'nil)
      (progn
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t))))

(defvar belden-tab-completions (make-hash-table :test 'equal))
(defun belden-populate-hash (hash pairs)
  (loop for keyvalue in pairs do
        (let ((key (car keyvalue)) (value (cadr keyvalue)))
          (puthash key value hash))))

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

(defun he-perlclass-beg ()
  (save-excursion (skip-chars-backward "A-Za-z0-9:") (point)))
(defun he-perlclass-end ()
  (save-excursion (skip-chars-forward "A-Za-z0-9:") (point)))
(fset 'belden-perlclass-complete
      (make-hippie-expand-function '(belden-try-expand-perlclass)))
(defvar perlclass-history nil
  "History list for commands asking for a perl class.")
(defvar perlclass-minibuffer-map (copy-keymap minibuffer-local-map))
(define-key perlclass-minibuffer-map [tab] 'belden-perlclass-complete)

