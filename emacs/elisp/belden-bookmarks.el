(provide 'belden-bookmarks)
(require 'bookmark)

(defun belden-bookmark ()
  "Manage your bookmarks:
.         => current-word
bookmark! => set default
bookmark+ => move
bookmark* => move
bookmark- => delete
"
  (interactive)
  (let (bookmark prompt toset tomove todel)
    (if (not (null belden-current-bookmark))
        (setq prompt (format "Bookmark (%s): " belden-current-bookmark))
      (progn
        (setq prompt "Bookmark: ")
        (setq toset t)))
    (setq bookmark (completing-read prompt
       bookmark-alist nil nil nil bookmark-history))
    (while (string-match "[!*+-]$" bookmark)
      (if (string-match "!$" bookmark) (setq toset t))
      (if (string-match "[*+]$" bookmark) (setq tomove t))
      (if (string-match "-$" bookmark) (setq todel t))
      (setq bookmark (substring bookmark 0 -1)))
    (if (string= bookmark ".") (setq bookmark (current-word)))
    (if (string= bookmark "")
        (if (null belden-current-bookmark)
            (error "You have no default bookmark")
          (setq bookmark belden-current-bookmark)))
    (if toset (setq belden-current-bookmark bookmark))
    (if todel
        (progn
          (bookmark-delete bookmark)
          (if (string= belden-current-bookmark bookmark)
              (setq belden-current-bookmark (car (bookmark-all-names)))))
      (if (and (assoc bookmark bookmark-alist)
               (not tomove))
          (bookmark-jump bookmark)
        (bookmark-set bookmark)))))

(defun belden-bookmark-jump ()
  (interactive)
  (if (null belden-current-bookmark)
      (error "You have no default bookmark")
    (bookmark-jump belden-current-bookmark)))

(defun drop-bookmarks ()
  (interactive)
  (setq belden-current-bookmark 'nil)
  (loop for bookmark in (bookmark-all-names) do
        (bookmark-delete bookmark)))
