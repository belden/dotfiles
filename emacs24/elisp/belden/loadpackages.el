(provide 'belden/loadpackages)
(require 'package)
(require 'cl)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar belden/required-packages
  '(
    magit
    cycle-themes
    ample-theme
    ample-zen-theme
    afternoon-theme
    clues-theme
    nav
    hide-lines
    go-mode
    rotate
    sotlisp ;; looks worth dissecting
    sos     ;; stackoverflow search
    list-processes+ ;; add process management to (list-processes)
    golden-ratio
    ) "a list of packages to ensure are installed at launch.")

(defun belden/packages-installed-p ()
  (loop for p in belden/required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

; check that all packages are installed
(unless (belden/packages-installed-p)
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message " done.")
  (dolist (p belden/required-packages)
	 (when (not (package-installed-p p))
	   (package-install p))))
